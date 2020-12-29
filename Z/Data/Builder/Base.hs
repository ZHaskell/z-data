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

  * When building a short strict 'Bytes' with 'build'\/'buildWith', we double the buffer
    each time buffer is full.

  * When building a large lazy @[Bytes]@ with 'buildChunks'\/'buildChunksWith',
    we insert a new chunk when buffer is full.


Most of the time using combinators from this module to build 'Builder' s is enough,
but in case of rolling something shining from the ground, keep an eye on correct 'BuildResult' handling.

-}

module Z.Data.Builder.Base
  ( -- * Builder type
    Builder(..)
  , append
  , Buffer(..), freezeBuffer
  , BuildResult(..)
  , BuildStep
   -- * Running a builder
  , build
  , buildWith
  , buildChunks
  , buildChunksWith
  , buildText
  , unsafeBuildText
    -- * Basic buiders
  , bytes
  , ensureN
  , writeN
   -- * Pritimive builders
  , encodePrim
  , encodePrimLE
  , encodePrimBE
  -- * More builders
  , stringModifiedUTF8, charModifiedUTF8, stringUTF8, charUTF8, string7, char7, word7, string8, char8, word8, text
  -- * Builder helpers
  , paren, curly, square, angle, quotes, squotes, colon, comma, intercalateVec, intercalateList
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits                          (unsafeShiftL, unsafeShiftR, (.&.))
import           Data.Word
import           Data.Int
import           GHC.CString                        (unpackCString#, unpackCStringUtf8#)
import           GHC.Exts                           hiding (build)
import           GHC.Stack
import           Data.Primitive.PrimArray
import           Z.Data.Array.Unaligned
import           Z.Data.ASCII
import qualified Z.Data.Text.Base                 as T
import qualified Z.Data.Text.UTF8Codec            as T
import qualified Z.Data.Vector.Base               as V
import qualified Z.Data.Vector                    as V
import           Z.Foreign
import           System.IO.Unsafe
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

-- | Helper type to help ghc unpack
--
data Buffer = Buffer {-# UNPACK #-} !(MutablePrimArray RealWorld Word8)  -- ^ the buffer content
                     {-# UNPACK #-} !Int  -- ^ writing offset

-- | Freeze buffer and return a 'V.Bytes'.
--
-- Note the mutable buffer array will be shrinked with 'shrinkMutablePrimArray', which may not
-- able to be reused.
freezeBuffer :: Buffer -> IO V.Bytes
{-# INLINE freezeBuffer #-}
freezeBuffer (Buffer buf offset) = do
    siz <- getSizeofMutablePrimArray buf
    when (offset < siz) (shrinkMutablePrimArray buf offset)
    !arr <- unsafeFreezePrimArray buf
    return (V.PrimVector arr 0 offset)

-- | @BuilderStep@ is a function that fill buffer under given conditions.
--
type BuildStep = Buffer -> IO BuildResult

-- | 'BuildSignal's abstract signals to the caller of a 'BuildStep'. There are
-- three signals: 'Done', 'BufferFull', or 'InsertBytes' signals
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
newtype Builder a = Builder { runBuilder :: (a -> BuildStep) -> BuildStep }

instance Show (Builder a) where
    show = show . build

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
    shrink b = (bytes . V.pack) <$> shrink (V.unpack (build b))

instance CoArbitrary (Builder ()) where
    coarbitrary = coarbitrary . build

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
    ensureN 4 (\ mba i -> T.encodeCharModifiedUTF8 mba i chr)

packAddrModified :: Addr# -> Builder ()
packAddrModified addr0# = copy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    copy addr# = do
        writeN len (\ mba i -> copyPtrToMutablePrimArray mba i (Ptr addr#) len)

append :: Builder a -> Builder b -> Builder b
{-# INLINE append #-}
append (Builder f) (Builder g) = Builder (\ k -> f ( \ _ ->  g k))

--------------------------------------------------------------------------------

-- | Write a 'V.Bytes'.
bytes :: V.Bytes -> Builder ()
{-# INLINE bytes #-}
bytes bs@(V.PrimVector arr s l) = Builder (\ k buffer@(Buffer buf offset) -> do
    siz <- getSizeofMutablePrimArray buf
    if siz - offset >= l
    then do
        copyPrimArray buf offset arr s l
        k () (Buffer buf (offset+l))
    else return (InsertBytes buffer bs (k ()))) -- bytes should be copied in outer handling

--------------------------------------------------------------------------------

-- | Shortcut to 'buildWith' 'V.defaultInitSize'.
build :: Builder a -> V.Bytes
{-# INLINE build #-}
build = buildWith V.defaultInitSize

-- | Build some bytes and validate if it's UTF8 bytes.
buildText :: HasCallStack => Builder a -> T.Text
{-# INLINE buildText #-}
buildText = T.validate . buildWith V.defaultInitSize

-- | Build some bytes assuming it's UTF8 encoding.
--
-- Be carefully use this function because you could constrcut illegal 'T.Text' values.
-- Check 'Z.Data.Text.ShowT' for UTF8 encoding builders. This functions is intended to
-- be used in debug only.
unsafeBuildText :: Builder a -> T.Text
{-# INLINE unsafeBuildText #-}
unsafeBuildText = T.Text . buildWith V.defaultInitSize

-- | Run Builder with doubling buffer strategy, which is suitable
-- for building short bytes.
buildWith :: Int -> Builder a -> V.Bytes
{-# INLINABLE buildWith #-}
buildWith initSiz (Builder b) = unsafePerformIO $ do
    buf <- newPrimArray initSiz
    loop =<< b (\ _ -> return . Done) (Buffer buf 0)
  where
    loop r = case r of
        Done buffer -> freezeBuffer buffer
        BufferFull (Buffer buf offset) wantSiz k -> do
            !siz <- getSizeofMutablePrimArray buf
            let !siz' = max (offset + wantSiz `unsafeShiftL` 1)
                            (siz `unsafeShiftL` 1)
            buf' <- resizeMutablePrimArray buf siz'   -- grow buffer
            loop =<< k (Buffer buf' offset)
        InsertBytes (Buffer buf offset) (V.PrimVector arr s l) k -> do
            !siz <- getSizeofMutablePrimArray buf
            let !siz' = max (offset + l `unsafeShiftL` 1)
                            (siz `unsafeShiftL` 1)
            buf' <- resizeMutablePrimArray buf siz'   -- grow buffer
            copyPrimArray buf' offset arr s l
            loop =<< k (Buffer buf' (offset+l))

-- | Shortcut to 'buildChunksWith' 'V.defaultChunkSize'.
buildChunks :: Builder a -> [V.Bytes]
{-# INLINE buildChunks #-}
buildChunks = buildChunksWith  V.smallChunkSize V.defaultChunkSize

-- | Run Builder with inserting chunk strategy, which is suitable
-- for building a list of bytes chunks and processing them in a streaming ways.
--
-- Note the building process is lazy, building happens when list chunks are consumed.
buildChunksWith :: Int -> Int -> Builder a -> [V.Bytes]
{-# INLINABLE buildChunksWith #-}
buildChunksWith initSiz chunkSiz (Builder b) = unsafePerformIO $ do
    buf <- newPrimArray initSiz
    loop =<< b (\ _ -> return . Done) (Buffer buf 0)
  where
    loop r = case r of
        Done buffer -> do
            !v <- freezeBuffer buffer
            return [v]
        BufferFull buffer@(Buffer _ offset) wantSiz k -> do
            let !siz' = max chunkSiz wantSiz
            buf' <- newPrimArray siz'   -- new buffer
            if offset == 0
            then loop =<< k (Buffer buf' 0)
            else do
                !v <- freezeBuffer buffer
                vs <- unsafeInterleaveIO . loop =<< k (Buffer buf' 0)
                return (v:vs)
        InsertBytes buffer@(Buffer _ offset) v@(V.PrimVector arr s l) k -> do
            if offset == 0
            then do
                vs <- unsafeInterleaveIO . loop =<< k buffer
                return (v:vs)
            else do
                !v' <- freezeBuffer buffer
                buf' <- newPrimArray chunkSiz   -- new buffer
                if l < chunkSiz `unsafeShiftR` 1
                then do
                    copyPrimArray buf' 0 arr s l
                    vs <- unsafeInterleaveIO . loop =<< k (Buffer buf' l)
                    return (v':vs)
                else do
                    vs <- unsafeInterleaveIO . loop =<< k (Buffer buf' 0)
                    return (v':v:vs)


--------------------------------------------------------------------------------

ensureN :: Int  -- ^ size bound
       -> (MutablePrimArray RealWorld Word8 -> Int -> IO Int)  -- ^ the writer which return a new offset
                                                               -- for next write
       -> Builder ()
{-# INLINE ensureN #-}
ensureN !n f = Builder (\ k buffer@(Buffer buf offset) -> do
    siz <- getSizeofMutablePrimArray buf
    if n + offset <= siz
    then f buf offset >>= \ offset' -> k () (Buffer buf offset')
    else return (BufferFull buffer n (\ (Buffer buf' offset') -> do
        f buf' offset' >>= \ offset'' -> k () (Buffer buf' offset''))))

writeN :: Int  -- ^ size bound
       -> (MutablePrimArray RealWorld Word8 -> Int -> IO ())  -- ^ the writer should write exactly N bytes
       -> Builder ()
{-# INLINE writeN #-}
writeN !n f = Builder (\ k buffer@(Buffer buf offset) -> do
    siz <- getSizeofMutablePrimArray buf
    if n + offset <= siz
    then f buf offset >> k () (Buffer buf (offset+n))
    else return (BufferFull buffer n (\ (Buffer buf' offset') -> do
        f buf' offset' >> k () (Buffer buf' (offset'+n)))))

-- | Write a primitive type in host byte order.
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
    writeN n (\ mpa i -> writePrimWord8ArrayAs mpa i x)
  where
    n = getUnalignedSize (unalignedSize @a)

-- | Write a primitive type with little endianess.
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

-- | Write a primitive type with big endianess.
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
        writeN len (\ mba i -> copyPtrToMutablePrimArray mba i (Ptr addr#) len)

packUTF8Addr :: Addr# -> Builder ()
packUTF8Addr addr0# = validateAndCopy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    valid = unsafeDupablePerformIO $ T.c_utf8_validate_addr addr0# len
    validateAndCopy addr#
        | valid == 0 = mapM_ charUTF8 (unpackCString# addr#)
        | otherwise = do
            writeN len (\ mba i -> copyPtrToMutablePrimArray mba i (Ptr addr#) len)

-- | Turn 'Char' into 'Builder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
charUTF8 :: Char -> Builder ()
{-# INLINE charUTF8 #-}
charUTF8 chr = do
    ensureN 4 (\ mba i -> T.encodeChar mba i chr)

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
char7 chr =
    writeN 1 (\ mpa i -> writePrimWord8ArrayAs mpa i (c2w chr .&. 0x7F))

-- | Turn 'Word8' into 'Builder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
word7 :: Word8 -> Builder ()
{-# INLINE word7 #-}
word7 w = writeN 1 (\ mpa i -> writePrimWord8ArrayAs mpa i (w .&. 0x7F))

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
    writeN 1 (\ mpa i -> writePrimWord8ArrayAs mpa i (c2w chr))

-- | Turn 'Word8' into 'Builder' with ASCII8 encoding, (alias to 'encodePrim').
--
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
word8 :: Word8 -> Builder ()
{-# INLINE word8 #-}
word8 = encodePrim

-- | Write UTF8 encoded 'Text' using 'Builder'.
--
-- Note, if you're trying to write string literals builders,
-- please open 'OverloadedStrings' and use 'Builder's 'IsString' instance,
-- it will be rewritten into a memcpy.
text :: T.Text -> Builder ()
{-# INLINE text #-}
text (T.Text bs) = bytes bs

--------------------------------------------------------------------------------


-- | add @(...)@ to original builder.
paren :: Builder () -> Builder ()
{-# INLINE paren #-}
paren b = encodePrim PAREN_LEFT >> b >> encodePrim PAREN_RIGHT

-- | add @{...}@ to original builder.
curly :: Builder () -> Builder ()
{-# INLINE curly #-}
curly b = encodePrim CURLY_LEFT >> b >> encodePrim CURLY_RIGHT

-- | add @[...]@ to original builder.
square :: Builder () -> Builder ()
{-# INLINE square #-}
square b = encodePrim SQUARE_LEFT >> b >> encodePrim SQUARE_RIGHT

-- | add @/<.../>@ to original builder.
angle :: Builder () -> Builder ()
{-# INLINE angle #-}
angle b = encodePrim ANGLE_LEFT >> b >> encodePrim ANGLE_RIGHT

-- | add @/".../"@ to original builder.
quotes :: Builder () -> Builder ()
{-# INLINE quotes #-}
quotes b = encodePrim QUOTE_DOUBLE >> b >> encodePrim QUOTE_DOUBLE

-- | add @/'.../'@ to original builder.
squotes :: Builder () -> Builder ()
{-# INLINE squotes #-}
squotes b = encodePrim QUOTE_SINGLE >> b >> encodePrim QUOTE_SINGLE

-- | write an ASCII @:@
colon :: Builder ()
{-# INLINE colon #-}
colon = encodePrim COLON

-- | write an ASCII @,@
comma :: Builder ()
{-# INLINE comma #-}
comma = encodePrim COMMA

-- | Use separator to connect a vector of builders.
--
-- @
-- import Z.Data.Builder as B
-- import Z.Data.Text    as T
-- import Z.Data.Vector  as V
--
-- > T.validate . B.build $ B.intercalateVec "," B.int (V.pack [1,2,3,4] :: V.PrimVector Int)
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
-- T.validate . B.build $ B.intercalateList "," B.int ([1,2,3,4] :: [Int])
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

