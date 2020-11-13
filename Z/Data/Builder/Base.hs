{-|
Module      : Z.Data.Builder.Base
Description : Efficient serialization/format.
Copyright   : (c) Dong Han, 2017-2019
              (c) Tao He, 2018-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

A 'Builder' records a buffer writing function, which can be 'mappend' in O(1) via composition.
In Z-Data a 'Builder' are designed to deal with different 'AllocateStrategy', it affects how
'Builder' react when writing across buffer boundaries:

  * When building a short strict 'Bytes' with 'buildBytes'\/'buildByteswith',
    we do a 'DoubleBuffer'.

  * When building a large lazy @[Bytes]@ with 'buildBytesList'\/'buildBytesListwith',
    we do an 'InsertChunk'.

  * When building and consuming are interlaced with 'buildAndRun'\/'buildAndRunWith',
    we do an 'OneShotAction'.

Most of the time using combinators from this module to build 'Builder' s is enough,
but in case of rolling something shining from the ground, keep an eye on correct
'AllocateStrategy' handling.

-}

module Z.Data.Builder.Base
  ( -- * Builder type
    Buffer(..)
  , BuildResult(..)
  , BuildStep
  , Builder(..)
  , append
   -- * Running a builder
  , buildBytes
  , buildBytesWith
  , buildBytesList
  , buildBytesListWith
    -- * Basic buiders
  , bytes -- hexBytes, hexBytesUpper
  , ensureN
  , atMost
  , writeN
   -- * Pritimive builders
  , encodePrim
  , encodePrimLE
  , encodePrimBE
  -- * More builders
  , stringModifiedUTF8, charModifiedUTF8, stringUTF8, charUTF8, string7, char7, string8, char8, text
  -- * Builder helpers
  , paren, curly, square, angle, quotes, squotes, colon, comma, intercalateVec, intercalateList
  -- * Hex helpers
  , hexEncodeBytes, hs_hex_encode, hs_hex_encode_upper
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits                          (unsafeShiftL, unsafeShiftR, (.&.))
import           Data.Primitive.Ptr                 (copyPtrToMutablePrimArray)
import           Data.Word
import           Data.Int
import           GHC.CString                        (unpackCString#, unpackCStringUtf8#)
import           GHC.Exts
import qualified Data.Primitive.PrimArray         as A
import           Z.Data.Array.Unaligned
import qualified Z.Data.Text.Base                 as T
import qualified Z.Data.Text.UTF8Codec            as T
import qualified Z.Data.Vector.Base               as V
import qualified Z.Data.Vector                    as V
import           Z.Foreign
import           System.IO.Unsafe
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

-- | Helper type to help ghc unpack
--
data Buffer = Buffer {-# UNPACK #-} !(A.MutablePrimArray RealWorld Word8)  -- ^ the buffer content
                     {-# UNPACK #-} !Int  -- ^ writing offset

freezeBuffer :: Buffer -> IO V.Bytes
{-# INLINE freezeBuffer #-}
freezeBuffer (Buffer buf offset) = do
    siz <- A.getSizeofMutablePrimArray buf
    when (offset < siz) (A.shrinkMutablePrimArray buf offset)
    !arr <- A.unsafeFreezePrimArray buf
    return (V.PrimVector arr 0 offset)

-- | @BuilderStep@ is a function that fill buffer under given conditions.
--
type BuildStep = Buffer -> IO BuildResult

-- | 'BuildSignal's abstract signals to the caller of a 'BuildStep'. There are
-- three signals: 'done', 'bufferFull', or 'insertChunks signals
data BuildResult
    = Done {-# UNPACK #-} !Buffer
    | BufferFull {-# UNPACK #-} !Buffer {-# UNPACK #-} !Int BuildStep
    | InsertBytes {-# UNPACK #-} !Buffer V.Bytes BuildStep

-- | @Builder@ is a monad to help compose @BuilderStep@. With next @BuilderStep@ continuation,
-- we can do interesting things like perform some action, or interleave the build process.
--
-- Notes on 'IsString' instance: @Builder ()@'s 'IsString' instance use 'stringModifiedUTF8',
-- which is different from 'stringUTF8' in that it DOES NOT PROVIDE UTF8 GUARANTEES! :
--
-- * @\\NUL@ will be written as @\xC0 \x80@.
-- * @\\xD800@ ~ @\\xDFFF@ will be encoded in three bytes as normal UTF-8 codepoints.
--
newtype Builder a = Builder
    { runBuilder :: (a -> BuildStep) -> BuildStep }

instance Show (Builder a) where
    show = show . buildBytes

instance Functor Builder where
    {-# INLINE fmap #-}
    fmap f (Builder b) = Builder (\ k -> b (k . f))
    {-# INLINE (<$) #-}
    a <$ (Builder b) = Builder (\ k -> b (\ _ -> k a))

instance Applicative Builder where
    {-# INLINE pure #-}
    pure x = Builder (\ k -> k x)
    {-# INLINE (<*>) #-}
    (Builder f) <*> (Builder b) = Builder (\ k -> f ( \ ab -> b (k . ab)))
    {-# INLINE (*>) #-}
    (*>) = append

instance Monad Builder where
    {-# INLINE (>>=) #-}
    (Builder b) >>= f = Builder (\ k -> b ( \ a -> runBuilder (f a) k))
    {-# INLINE (>>) #-}
    (>>) = append

instance Semigroup (Builder ()) where
    (<>) = append
    {-# INLINE (<>) #-}

instance Monoid (Builder ()) where
    mempty = pure ()
    {-# INLINE mempty #-}
    mappend = append
    {-# INLINE mappend #-}
    mconcat = foldr append (pure ())
    {-# INLINE mconcat #-}

-- | This instance simple write literals' bytes into buffer,
-- which is different from 'stringUTF8' in that it DOES NOT PROVIDE UTF8 GUARANTEES! :
instance (a ~ ()) => IsString (Builder a) where
    {-# INLINE fromString #-}
    fromString = stringModifiedUTF8

instance Arbitrary (Builder ()) where
    arbitrary = bytes <$> arbitrary
    shrink b = (bytes . V.pack) <$> shrink (V.unpack (buildBytes b))

instance CoArbitrary (Builder ()) where
    coarbitrary = coarbitrary . buildBytes

-- | Encode string with modified UTF-8 encoding, will be rewritten to a memcpy if possible.
stringModifiedUTF8 :: String -> Builder ()
{-# INLINE CONLIKE [0] stringModifiedUTF8 #-}
{-# RULES
    "stringModifiedUTF8/packAddrModified" forall addr . stringModifiedUTF8 (unpackCString# addr) = packAddrModified addr
  #-}
{-# RULES
    "stringModifiedUTF8/packAddrModified" forall addr . stringModifiedUTF8 (unpackCStringUtf8# addr) = packAddrModified addr
  #-}
stringModifiedUTF8 = mapM_ charModifiedUTF8

-- | Turn 'Char' into 'Builder' with Modified UTF8 encoding
--
-- @\\NUL@ is encoded as two bytes @C0 80@ , @\\xD800@ ~ @\\xDFFF@ is encoded as a three bytes normal UTF-8 codepoint.
charModifiedUTF8 :: Char -> Builder ()
{-# INLINE charModifiedUTF8 #-}
charModifiedUTF8 chr = do
    ensureN 4
    Builder (\ k (Buffer mba i) -> do
        i' <- T.encodeCharModifiedUTF8 mba i chr
        k () (Buffer mba i'))

packAddrModified :: Addr# -> Builder ()
packAddrModified addr0# = copy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    copy addr# = do
        ensureN len
        Builder (\ k (Buffer mba i) -> do
           copyPtrToMutablePrimArray mba i (Ptr addr#) len
           k () (Buffer mba (i + len)))

append :: Builder a -> Builder b -> Builder b
{-# INLINE append #-}
append (Builder f) (Builder g) = Builder (\ k -> f ( \ _ ->  g k))

--------------------------------------------------------------------------------

-- | Write a 'V.Bytes'.
bytes :: V.Bytes -> Builder ()
{-# INLINE bytes #-}
bytes bs@(V.PrimVector arr s l) = Builder (\ k buffer@(Buffer buf offset) -> do
    siz <- A.getSizeofMutablePrimArray buf
    if siz - offset >= l
    then do
        A.copyPrimArray buf offset arr s l
        k () (Buffer buf (offset+l))
    else return (InsertBytes buffer bs (k ())))

-- | Ensure that there are at least @n@ many elements available.
ensureN :: Int -> Builder ()
{-# INLINE ensureN #-}
ensureN !n = Builder (\ k buffer@(Buffer buf offset) -> do
    siz <- A.getSizeofMutablePrimArray buf
    if siz - offset >= n
    then k () buffer
    else return (BufferFull buffer n (k ())))

--------------------------------------------------------------------------------

-- | Shortcut to 'buildBytesWith' 'V.defaultInitSize'.
buildBytes :: Builder a -> V.Bytes
{-# INLINE buildBytes #-}
buildBytes = buildBytesWith V.defaultInitSize

-- | Run Builder with 'DoubleBuffer' strategy, which is suitable
-- for building short bytes.
buildBytesWith :: Int -> Builder a -> V.Bytes
{-# INLINABLE buildBytesWith #-}
buildBytesWith initSiz (Builder b) = unsafePerformIO $ do
    buf <- A.newPrimArray initSiz
    loop =<< b (\ _ -> return . Done) (Buffer buf 0)
  where
    loop r = case r of
        Done buffer -> freezeBuffer buffer
        BufferFull (Buffer buf offset) wantSiz k -> do
            !siz <- A.getSizeofMutablePrimArray buf
            let !siz' = max (offset + wantSiz `unsafeShiftL` 1)
                            (siz `unsafeShiftL` 1)
            buf' <- A.resizeMutablePrimArray buf siz'   -- grow buffer
            loop =<< k (Buffer buf' offset)
        InsertBytes (Buffer buf offset) (V.PrimVector arr s l) k -> do
            !siz <- A.getSizeofMutablePrimArray buf
            let !siz' = max (offset + l `unsafeShiftL` 1)
                            (siz `unsafeShiftL` 1)
            buf' <- A.resizeMutablePrimArray buf siz'   -- grow buffer
            A.copyPrimArray buf' offset arr s l
            loop =<< k (Buffer buf' (offset+l))

-- | Shortcut to 'buildBytesListWith' 'V.defaultChunkSize'.
buildBytesList :: Builder a -> [V.Bytes]
{-# INLINE buildBytesList #-}
buildBytesList = buildBytesListWith  V.smallChunkSize V.defaultChunkSize

-- | Run Builder with 'InsertChunk' strategy, which is suitable
-- for building lazy bytes chunks.
buildBytesListWith :: Int -> Int -> Builder a -> [V.Bytes]
{-# INLINABLE buildBytesListWith #-}
buildBytesListWith initSiz chunkSiz (Builder b) = unsafePerformIO $ do
    buf <- A.newPrimArray initSiz
    loop [] =<< b (\ _ -> return . Done) (Buffer buf 0)
  where
    loop acc r = case r of
        Done buffer -> do
            !v <- freezeBuffer buffer
            return (reverse (v : acc))
        BufferFull buffer@(Buffer buf offset) wantSiz k -> do
            let !siz' = max chunkSiz wantSiz
            buf' <- A.resizeMutablePrimArray buf siz'   -- new buffer
            if (offset == 0)
            then loop acc =<< k (Buffer buf' 0)
            else do
                !v <- freezeBuffer buffer
                loop (v : acc) =<< k (Buffer buf' 0)
        InsertBytes buffer@(Buffer buf offset) v@(V.PrimVector arr s l) k -> do
            if (offset == 0)
            then loop (v : acc) =<< k buffer
            else do
                !v' <- freezeBuffer buffer
                buf' <- A.resizeMutablePrimArray buf chunkSiz   -- new buffer
                if (l < chunkSiz `unsafeShiftR` 1)
                then do
                    A.copyPrimArray buf' 0 arr s l
                    loop (v' : acc) =<< k (Buffer buf' l)
                else loop (v : v' : acc) =<< k (Buffer buf' 0)

--------------------------------------------------------------------------------

atMost :: Int  -- ^ size bound
       -> (A.MutablePrimArray RealWorld Word8 -> Int -> IO Int)  -- ^ the writer which pure a new offset
                                                                       -- for next write
       -> Builder ()
{-# INLINE atMost #-}
atMost n f = ensureN n `append`
    Builder (\ k (Buffer buf offset ) ->
        f buf offset >>= \ offset' -> k () (Buffer buf offset'))

writeN :: Int  -- ^ size bound
       -> (A.MutablePrimArray RealWorld Word8 -> Int -> IO ())  -- ^ the writer which pure a new offset
                                                                    -- for next write
       -> Builder ()
{-# INLINE writeN #-}
writeN n f = ensureN n `append`
    Builder (\ k (Buffer buf offset ) ->
        f buf offset >> k () (Buffer buf (offset+n)))

-- | write primitive types in host byte order.
encodePrim :: forall a. Unaligned a => a -> Builder ()
{-# INLINE encodePrim #-}
{-# SPECIALIZE INLINE encodePrim :: Word -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Word64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Word8 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int16 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrim :: Int8 -> Builder () #-}
encodePrim x = do
    ensureN n
    Builder (\ k (Buffer mpa i) -> do
        writePrimWord8ArrayAs mpa i x
        k () (Buffer mpa (i + n)))
  where
    n = unalignedSize (undefined :: a)

-- | write primitive types with little endianess.
encodePrimLE :: forall a. Unaligned (LE a) => a -> Builder ()
{-# INLINE encodePrimLE #-}
{-# SPECIALIZE INLINE encodePrimLE :: Word -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Word64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Int -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Int64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Int32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimLE :: Int16 -> Builder () #-}
encodePrimLE = encodePrim . LE

-- | write primitive types with big endianess.
encodePrimBE :: forall a. Unaligned (BE a) => a -> Builder ()
{-# INLINE encodePrimBE #-}
{-# SPECIALIZE INLINE encodePrimBE :: Word -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Word64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Int -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Int64 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Int32 -> Builder () #-}
{-# SPECIALIZE INLINE encodePrimBE :: Int16 -> Builder () #-}
encodePrimBE = encodePrim . BE

--------------------------------------------------------------------------------

-- | Turn 'String' into 'Builder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
--
-- This is different from writing string literals builders via @OverloadedStrings@, because string literals
-- do not provide UTF8 guarantees.
--
-- This function will be rewritten into a memcpy if possible, (running a fast UTF-8 validation
-- at runtime first).
stringUTF8 :: String -> Builder ()
{-# INLINE CONLIKE [0] stringUTF8 #-}
{-# RULES
    "stringUTF8/packASCIIAddr" forall addr . stringUTF8 (unpackCString# addr) = packASCIIAddr addr
  #-}
{-# RULES
    "stringUTF8/packUTF8Addr" forall addr . stringUTF8 (unpackCString# addr) = packUTF8Addr addr
  #-}
stringUTF8 = mapM_ charUTF8

packASCIIAddr :: Addr# -> Builder ()
packASCIIAddr addr0# = copy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    copy addr# = do
        ensureN len
        Builder (\ k (Buffer mba i) -> do
           copyPtrToMutablePrimArray mba i (Ptr addr#) len
           k () (Buffer mba (i + len)))

packUTF8Addr :: Addr# -> Builder ()
packUTF8Addr addr0# = validateAndCopy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    valid = unsafeDupablePerformIO $ T.c_utf8_validate_addr addr0# len
    validateAndCopy addr#
        | valid == 0 = mapM_ charUTF8 (unpackCString# addr#)
        | otherwise = do
            ensureN len
            Builder (\ k (Buffer mba i) -> do
               copyPtrToMutablePrimArray mba i (Ptr addr#) len
               k () (Buffer mba (i + len)))

-- | Turn 'Char' into 'Builder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
charUTF8 :: Char -> Builder ()
{-# INLINE charUTF8 #-}
charUTF8 chr = do
    ensureN 4
    Builder (\ k (Buffer mba i) -> do
        i' <- T.encodeChar mba i chr
        k () (Buffer mba i'))

-- | Turn 'String' into 'Builder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
string7 :: String -> Builder ()
{-# INLINE string7 #-}
string7 = mapM_ char7

-- | Turn 'Char' into 'Builder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
char7 :: Char -> Builder ()
{-# INLINE char7 #-}
char7 chr = do
    ensureN 1
    Builder (\ k (Buffer mpa i) -> do
        let x = V.c2w chr .&. 0x7F
        writePrimWord8ArrayAs mpa i x
        k () (Buffer mpa (i+1)))

-- | Turn 'String' into 'Builder' with ASCII8 encoding
--
-- Codepoints beyond @'\xFF'@ will be chopped.
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
string8 :: String -> Builder ()
{-# INLINE string8 #-}
string8 = mapM_ char8

-- | Turn 'Char' into 'Builder' with ASCII8 encoding
--
-- Codepoints beyond @'\xFF'@ will be chopped.
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
char8 :: Char -> Builder ()
{-# INLINE char8 #-}
char8 chr = do
    ensureN 1
    Builder (\ k (Buffer mpa i) -> do
        let x = V.c2w chr
        writePrimWord8ArrayAs mpa i x
        k () (Buffer mpa (i+1)))

-- | Write UTF8 encoded 'Text' using 'Builder'.
--
-- Note, if you're trying to write string literals builders,
-- please open 'OverloadedStrings' and use 'Builder's 'IsString' instance,
-- it will be rewritten into a memcpy.
text :: T.Text -> Builder ()
{-# INLINE text #-}
text (T.Text bs) = bytes bs

--------------------------------------------------------------------------------

#define BACKSLASH 92
#define CLOSE_ANGLE 62
#define CLOSE_CURLY 125
#define CLOSE_PAREN 41
#define CLOSE_SQUARE 93
#define COMMA 44
#define COLON 58
#define DOUBLE_QUOTE 34
#define OPEN_ANGLE 60
#define OPEN_CURLY 123
#define OPEN_PAREN 40
#define OPEN_SQUARE 91
#define SINGLE_QUOTE 39

-- | add @(...)@ to original builder.
paren :: Builder () -> Builder ()
{-# INLINE paren #-}
paren b = encodePrim @Word8 OPEN_PAREN >> b >> encodePrim @Word8 CLOSE_PAREN

-- | add @{...}@ to original builder.
curly :: Builder () -> Builder ()
{-# INLINE curly #-}
curly b = encodePrim @Word8 OPEN_CURLY >> b >> encodePrim @Word8 CLOSE_CURLY

-- | add @[...]@ to original builder.
square :: Builder () -> Builder ()
{-# INLINE square #-}
square b = encodePrim @Word8 OPEN_SQUARE >> b >> encodePrim @Word8 CLOSE_SQUARE

-- | add @/<.../>@ to original builder.
angle :: Builder () -> Builder ()
{-# INLINE angle #-}
angle b = encodePrim @Word8 OPEN_ANGLE >> b >> encodePrim @Word8 CLOSE_ANGLE

-- | add @/".../"@ to original builder.
quotes :: Builder () -> Builder ()
{-# INLINE quotes #-}
quotes b = encodePrim @Word8 DOUBLE_QUOTE >> b >> encodePrim @Word8 DOUBLE_QUOTE

-- | add @/'.../'@ to original builder.
squotes :: Builder () -> Builder ()
{-# INLINE squotes #-}
squotes b = encodePrim @Word8 SINGLE_QUOTE >> b >> encodePrim @Word8 SINGLE_QUOTE

-- | write an ASCII @:@
colon :: Builder ()
{-# INLINE colon #-}
colon = encodePrim @Word8 COLON

-- | write an ASCII @,@
comma :: Builder ()
{-# INLINE comma #-}
comma = encodePrim @Word8 COMMA

-- | Use separator to connect a vector of builders.
--
-- @
-- import Z.Data.Builder as B
-- import Z.Data.Text    as T
-- import Z.Data.Vector  as V
--
-- > T.validate . B.buildBytes $ B.intercalateVec "," B.int (V.pack [1,2,3,4] :: V.PrimVector Int)
-- "1,2,3,4"
-- @
intercalateVec :: (V.Vec v a)
            => Builder ()           -- ^ the seperator
            -> (a -> Builder ())    -- ^ value formatter
            -> v a                  -- ^ value vector
            ->  Builder ()
{-# INLINE intercalateVec #-}
intercalateVec s f v = do
    V.traverseVec_ (\ x -> f x >> s) (V.initMayEmpty v)
    forM_ (V.lastMaybe v) f

-- | Use separator to connect list of builders.
--
-- @
-- import Z.Data.Builder as B
-- import Z.Data.Text    as T
-- import Z.Data.Vector  as V
--
-- T.validate . B.buildBytes $ B.intercalateList "," B.int ([1,2,3,4] :: [Int])
-- "1,2,3,4"
-- @
intercalateList :: Builder ()           -- ^ the seperator
                -> (a -> Builder ())    -- ^ value formatter
                -> [a]                  -- ^ value list
                -> Builder ()
{-# INLINE intercalateList #-}
intercalateList s f xs = go xs
  where
    go [] = pure ()
    go [x] = f x
    go (x:xs') = f x >> s >> go xs'

-- | Encode 'V.Bytes' using hex(base16) encoding.
hexEncodeBytes :: Bool      -- ^ uppercase?
               -> V.Bytes -> V.Bytes
hexEncodeBytes upper (V.PrimVector arr s l) = fst . unsafeDupablePerformIO $ do
    allocPrimVectorUnsafe (l `unsafeShiftL` 1) $ \ buf# ->
        withPrimArrayUnsafe arr $ \ parr _ ->
            if upper
            then hs_hex_encode_upper buf# 0 parr s l
            else hs_hex_encode buf# 0 parr s l

foreign import ccall unsafe hs_hex_encode :: MBA# Word8 -> Int -> BA# Word8 -> Int -> Int -> IO ()
foreign import ccall unsafe hs_hex_encode_upper :: MBA# Word8 -> Int -> BA# Word8 -> Int -> Int -> IO ()
