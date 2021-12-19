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

  * When building a large strict @[Bytes]@ with 'buildChunks'\/'buildChunksWith',
    we insert a new chunk when buffer is full.

-}

module Z.Data.Builder.Base
  ( -- * Builder type
    Builder(..)
  , append
  , Buffer(..), freezeBuffer
   -- * Running a builder
  , build
  , buildWith
  , buildAndRun
  , buildAndRunWith
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
  , BE(..), LE(..)
  , encodePrimLE
  , encodePrimBE
  -- * More builders
  , stringModifiedUTF8, charModifiedUTF8, stringUTF8
  , charUTF8, string7, char7, word7, string8, char8, word8, word8N, text
  -- * Builder helpers
  , paren, parenWhen, curly, square, angle, quotes, squotes, colon, comma, intercalateVec, intercalateList
    -- * Specialized primitive builder
  , encodeWord  , encodeWord64, encodeWord32, encodeWord16, encodeWord8
  , encodeInt   , encodeInt64 , encodeInt32 , encodeInt16 , encodeInt8 , encodeDouble, encodeFloat
  , encodeWordLE  , encodeWord64LE , encodeWord32LE , encodeWord16LE
  , encodeIntLE   , encodeInt64LE , encodeInt32LE , encodeInt16LE , encodeDoubleLE , encodeFloatLE
  , encodeWordBE  , encodeWord64BE , encodeWord32BE , encodeWord16BE
  , encodeIntBE   , encodeInt64BE , encodeInt32BE , encodeInt16BE , encodeDoubleBE , encodeFloatBE
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits                          (unsafeShiftL, unsafeShiftR, (.&.))
import           Data.Primitive.Ptr                 (copyPtrToMutablePrimArray)
import           Data.Word
import           Data.IORef
import           Data.Int
import           GHC.Exts                           hiding (build)
import           GHC.Stack
import           Data.Primitive.PrimArray
import           Z.Data.Array.Unaligned
import           Z.Data.PrimRef
import           Z.Data.ASCII
import qualified Z.Data.Text.Base                   as T
import qualified Z.Data.Text.UTF8Codec              as T
import qualified Z.Data.Vector.Base                 as V
import qualified Z.Data.Array                       as A
import           Prelude                            hiding (encodeFloat)
import           System.IO.Unsafe
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

-- | Helper type to help ghc unpack
--
data Buffer = Buffer
  { bufferRef :: {-# UNPACK #-} !(UnliftedIORef (MutablePrimArray RealWorld Word8)) -- ^ the buffer content
  , offsetRef :: {-# UNPACK #-} !Counter                -- ^ writing offset
  , onBufferFull :: Int -> Maybe V.Bytes -> IO ()    -- ^ called when buffer boundary is hit
  }

-- | Freeze buffer and return a 'V.Bytes'.
--
-- Note the mutable buffer array will be shrinked with 'shrinkMutablePrimArray', which may not
-- able to be reused.
freezeBuffer :: UnliftedIORef (MutablePrimArray RealWorld Word8) -> Counter -> IO V.Bytes
{-# INLINE freezeBuffer #-}
freezeBuffer bufRef offRef = do
    buf <- readUnliftedRef bufRef
    off <- readPrimRef offRef
    siz <- getSizeofMutablePrimArray buf
    when (off < siz) (shrinkMutablePrimArray buf off)
    !arr <- unsafeFreezePrimArray buf
    return (V.PrimVector arr 0 off)

-- | @Builder@ is a monad to help compose 'Buffer' writing functions.
--
-- Notes on 'IsString' instance: @Builder ()@'s 'IsString' instance use 'stringModifiedUTF8',
-- which is different from 'stringUTF8' in that it DOES NOT PROVIDE UTF8 GUARANTEES! :
--
-- * @\\NUL@ will be written as @\\xC0 \\x80@.
-- * @\\xD800@ ~ @\\xDFFF@ will be encoded in three bytes as normal UTF-8 codepoints.
--
newtype Builder a = Builder (Buffer -> IO a)

instance Show (Builder a) where
    show = show . build

instance Functor Builder where
    {-# INLINE fmap #-}
    fmap f (Builder b) = Builder (\ buf -> fmap f (b buf))
    {-# INLINE (<$) #-}
    a <$ (Builder b) = Builder (\ buf -> b buf >> pure a)

instance Applicative Builder where
    {-# INLINE pure #-}
    pure x = Builder (\ _ -> pure x)
    {-# INLINE (<*>) #-}
    (Builder f) <*> (Builder g) = Builder (\ buf -> do
        f' <- f buf
        g' <- g buf
        pure (f' g'))
    {-# INLINE (*>) #-}
    (*>) = append

instance Monad Builder where
    {-# INLINE (>>=) #-}
    (Builder b) >>= f = Builder (\ buf -> do
        x <- b buf
        let Builder b' = f x
        b' buf)
    {-# INLINE (>>) #-}
    (>>) = (*>)

instance Semigroup (Builder ()) where
    (<>) = append
    {-# INLINE (<>) #-}

instance Monoid (Builder ()) where
    mempty = pure ()
    {-# INLINE mempty #-}
    mappend = (<>)
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
{-# INLINE packAddrModified #-}
packAddrModified addr0# = copy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    copy addr# = do
        writeN len (\ mba i -> copyPtrToMutablePrimArray mba i (Ptr addr#) len)

append :: Builder a -> Builder b -> Builder b
{-# INLINE append #-}
append (Builder f) (Builder g) = Builder (\ buf -> f buf >> g buf)

--------------------------------------------------------------------------------

-- | Write a 'V.Bytes'.
bytes :: V.Bytes -> Builder ()
{-# INLINE bytes #-}
bytes v@(V.PrimVector arr s l) = Builder (\ (Buffer bufRef offRef onBufFull) -> do
    buf <- readUnliftedRef bufRef
    off <- readPrimRef offRef
    siz <- getSizeofMutablePrimArray buf
    let off' = off + l
    if off' <= siz
    then do
        copyPrimArray buf off arr s l
        writePrimRef offRef off'
    else do
        onBufFull 0 (Just v))

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
    bufRef <- newUnliftedRef =<< newPrimArray initSiz
    offRef <- newCounter 0
    let buffer = Buffer bufRef offRef (doubleBuffer bufRef offRef)
    _ <- b buffer
    freezeBuffer bufRef offRef
  where
    doubleBuffer bufRef offRef _ (Just (V.PrimVector arr s l)) = do
        buf <- readUnliftedRef bufRef
        off <- readPrimRef offRef
        !siz <- getSizeofMutablePrimArray buf
        let !siz' = max (off + l `unsafeShiftL` 1) (siz `unsafeShiftL` 1)
        buf' <- resizeMutablePrimArray buf siz'   -- grow buffer
        copyPrimArray buf' off arr s l
        writeUnliftedRef bufRef buf'
        writePrimRef offRef (off + l)
    doubleBuffer bufRef offRef wantSiz _ = do
        buf <- readUnliftedRef bufRef
        off <- readPrimRef offRef
        !siz <- getSizeofMutablePrimArray buf
        let !siz' = max (off + wantSiz `unsafeShiftL` 1)
                        (siz `unsafeShiftL` 1)
        buf' <- resizeMutablePrimArray buf siz'   -- grow buffer
        writeUnliftedRef bufRef buf'

-- | Shortcut to 'buildAndRunWith' 'V.defaultChunkSize'.
buildAndRun :: Builder a -> (V.Bytes -> IO ()) -> IO ()
{-# INLINE buildAndRun #-}
buildAndRun = buildAndRunWith V.defaultChunkSize

-- | Run Builder with inserting chunk strategy, which is suitable
-- for processing them in a streaming ways.
--
-- Note this functions DO NO REUSE buffers, you can safely save the result chunks for later use.
buildAndRunWith :: Int -> Builder a -> (V.Bytes -> IO ()) -> IO ()
{-# INLINABLE buildAndRunWith #-}
buildAndRunWith chunkSiz (Builder b) action = do
    bufRef <- newUnliftedRef =<< newPrimArray chunkSiz
    offRef <- newCounter 0
    let buffer = Buffer bufRef offRef (switchBuffer bufRef offRef)
    _ <- b buffer
    off <- readPrimRef offRef
    when (off /= 0) (do
        !v <- freezeBuffer bufRef offRef
        action v)
  where
    switchBuffer bufRef offRef _ (Just v@(V.PrimVector arr s l)) = do
        off <- readPrimRef offRef
        if off == 0
        then do
          -- buffer is OK, but inserted chunk is too large to fit
          action v
        else do
            !v' <- freezeBuffer bufRef offRef
            buf' <- newPrimArray chunkSiz   -- new buffer
            writeUnliftedRef bufRef buf'
            if l < chunkSiz `unsafeShiftR` 1
            then do
                -- copy inserted chunk to new buffer
                copyPrimArray buf' 0 arr s l
                writePrimRef offRef l
                action v'
            else do
                -- prepend inserted chunk to list
                writePrimRef offRef 0
                action v'
                action v

    switchBuffer bufRef offRef wantSiz _ = do
        off <- readPrimRef offRef
        when (off /= 0) (do
            !v <- freezeBuffer bufRef offRef
            action v)
        let !siz' = max chunkSiz wantSiz
        buf' <- newPrimArray siz'   -- new buffer
        writeUnliftedRef bufRef buf'
        writePrimRef offRef 0

-- | Shortcut to 'buildChunksWith' 'V.defaultChunkSize'.
buildChunks :: Builder a -> [V.Bytes]
{-# INLINE buildChunks #-}
buildChunks = buildChunksWith V.defaultChunkSize

-- | Run Builder with inserting chunk strategy, which is suitable
-- for building a list of bytes chunks.
--
-- Note the building process is strict, all list chunks will stay in memory during building.
buildChunksWith :: Int -> Builder a -> [V.Bytes]
{-# INLINABLE buildChunksWith #-}
buildChunksWith chunkSiz (Builder b) = unsafePerformIO $ do
    bufRef <- newUnliftedRef =<< newPrimArray chunkSiz
    offRef <- newCounter 0
    bufListRef <- newIORef []
    let buffer = (Buffer bufRef offRef (switchBuffer bufListRef bufRef offRef))
    _ <- b buffer
    off <- readPrimRef offRef
    when (off /= 0) (do
        !v <- freezeBuffer bufRef offRef
        modifyIORef bufListRef (v:))
    reverse <$> readIORef bufListRef
  where
    switchBuffer bufListRef bufRef offRef _ (Just v@(V.PrimVector arr s l)) = do
        off <- readPrimRef offRef
        if off == 0
        then do
          -- buffer is OK, but inserted chunk is too large to fit
          modifyIORef bufListRef (v:)
        else do
            !v' <- freezeBuffer bufRef offRef
            buf' <- newPrimArray chunkSiz   -- new buffer
            writeUnliftedRef bufRef buf'
            if l < chunkSiz `unsafeShiftR` 1
            then do
                -- copy inserted chunk to new buffer
                copyPrimArray buf' 0 arr s l
                writePrimRef offRef l
                modifyIORef bufListRef (v':)
            else do
                -- prepend inserted chunk to list
                writePrimRef offRef 0
                modifyIORef bufListRef (\vs -> v:v':vs)

    switchBuffer bufListRef bufRef offRef wantSiz _ = do
        off <- readPrimRef offRef
        when (off /= 0) (do
            !v <- freezeBuffer bufRef offRef
            modifyIORef bufListRef (v:))
        let !siz' = max chunkSiz wantSiz
        buf' <- newPrimArray siz'   -- new buffer
        writeUnliftedRef bufRef buf'
        writePrimRef offRef 0

--------------------------------------------------------------------------------

ensureN :: Int  -- ^ size bound
       -> (MutablePrimArray RealWorld Word8 -> Int -> IO Int)  -- ^ the writer which return a new offset
                                                               -- for next write
       -> Builder ()
{-# INLINE ensureN #-}
ensureN !n f = Builder (\ (Buffer bufRef offRef onBufFull) -> do
    buf <- readUnliftedRef bufRef
    off <- readPrimRef offRef
    siz <- getSizeofMutablePrimArray buf
    if ((n + off) <= siz)
    then do
        off' <- f buf off
        writePrimRef offRef off'
    else do
        onBufFull n Nothing
        buf' <- readUnliftedRef bufRef
        off' <- readPrimRef offRef
        off'' <- f buf' off'
        writePrimRef offRef off'')

writeN :: Int  -- ^ size bound
       -> (MutablePrimArray RealWorld Word8 -> Int -> IO ())  -- ^ the writer should write exactly N bytes
       -> Builder ()
{-# INLINE writeN #-}
writeN !n f = Builder (\ (Buffer bufRef offRef onBufFull) -> do
    buf <- readUnliftedRef bufRef
    off <- readPrimRef offRef
    siz <- getSizeofMutablePrimArray buf
    let off' = off + n
    if (off' <= siz)
    then do
        f buf off
        writePrimRef offRef off'
    else do
        onBufFull n Nothing
        buf' <- readUnliftedRef bufRef
        off'' <- readPrimRef offRef
        f buf' off''
        writePrimRef offRef (off''+n))

{- These rules are bascially what inliner do so no need to mess up with them
{-# RULES
  "ensureN/merge" forall n1 f1 n2 f2. append (ensureN n1 f1) (ensureN n2 f2) = ensureN (n1 + n2) (\ mba i -> f1 mba i >>= \ i' -> f2 mba i') #-}
{-# RULES
  "writeN/merge" forall n1 f1 n2 f2. append (writeN n1 f1) (writeN n2 f2) = writeN (n1 + n2) (\ mba i -> f1 mba i >> f2 mba (i+n1)) #-}
-}

-- | Write a primitive type in host byte order.
--
-- @
-- > encodePrim (256 :: Word16, BE 256 :: BE Word16)
-- > [0,1,1,0]
-- @
encodePrim :: forall a. Unaligned a => a -> Builder ()
{-# INLINE encodePrim #-}
encodePrim x = do
    writeN n (\ mpa i -> writePrimWord8ArrayAs mpa i x)
  where
    n = getUnalignedSize (unalignedSize @a)

#define ENCODE_HOST(f, type) \
    f :: type -> Builder (); {-# INLINE f #-}; f = encodePrim; \
    -- ^ Encode type in host endian order.

ENCODE_HOST(encodeWord  , Word   )
ENCODE_HOST(encodeWord64, Word64 )
ENCODE_HOST(encodeWord32, Word32 )
ENCODE_HOST(encodeWord16, Word16 )
ENCODE_HOST(encodeWord8 , Word8  )
ENCODE_HOST(encodeInt   , Int    )
ENCODE_HOST(encodeInt64 , Int64  )
ENCODE_HOST(encodeInt32 , Int32  )
ENCODE_HOST(encodeInt16 , Int16  )
ENCODE_HOST(encodeInt8  , Int8   )
ENCODE_HOST(encodeDouble, Double )
ENCODE_HOST(encodeFloat , Float  )

-- | Write a primitive type with little endianess.
encodePrimLE :: forall a. Unaligned (LE a) => a -> Builder ()
{-# INLINE encodePrimLE #-}
encodePrimLE = encodePrim . LE

#define ENCODE_LE(f, type) \
    f :: type -> Builder (); {-# INLINE f #-}; f = encodePrimLE; \
    -- ^ Encode type in little endian order.

ENCODE_LE(encodeWordLE  , Word   )
ENCODE_LE(encodeWord64LE, Word64 )
ENCODE_LE(encodeWord32LE, Word32 )
ENCODE_LE(encodeWord16LE, Word16 )
ENCODE_LE(encodeIntLE   , Int    )
ENCODE_LE(encodeInt64LE , Int64  )
ENCODE_LE(encodeInt32LE , Int32  )
ENCODE_LE(encodeInt16LE , Int16  )
ENCODE_LE(encodeDoubleLE, Double )
ENCODE_LE(encodeFloatLE , Float  )

-- | Write a primitive type with big endianess.
encodePrimBE :: forall a. Unaligned (BE a) => a -> Builder ()
{-# INLINE encodePrimBE #-}
encodePrimBE = encodePrim . BE

#define ENCODE_BE(f, type) \
    f :: type -> Builder (); {-# INLINE f #-}; f = encodePrimBE; \
    -- ^ Encode type in little endian order.

ENCODE_BE(encodeWordBE  , Word   )
ENCODE_BE(encodeWord64BE, Word64 )
ENCODE_BE(encodeWord32BE, Word32 )
ENCODE_BE(encodeWord16BE, Word16 )
ENCODE_BE(encodeIntBE   , Int    )
ENCODE_BE(encodeInt64BE , Int64  )
ENCODE_BE(encodeInt32BE , Int32  )
ENCODE_BE(encodeInt16BE , Int16  )
ENCODE_BE(encodeDoubleBE, Double )
ENCODE_BE(encodeFloatBE , Float  )

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
{-# INLINE packASCIIAddr #-}
packASCIIAddr addr0# = copy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    copy addr# = do
        writeN len (\ mba i -> copyPtrToMutablePrimArray mba i (Ptr addr#) len)

packUTF8Addr :: Addr# -> Builder ()
{-# INLINABLE packUTF8Addr #-}
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
char7 chr = writeN 1 (\ mpa i -> writePrimWord8ArrayAs mpa i (c2w chr .&. 0x7F))

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
char8 chr = writeN 1 (\ mpa i -> writePrimWord8ArrayAs mpa i (c2w chr))

-- | Turn 'Word8' into 'Builder' with ASCII8 encoding, (alias to 'encodePrim').
--
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
word8 :: Word8 -> Builder ()
{-# INLINE word8 #-}
word8 = encodePrim

-- | Faster version of @replicateM x . word8@ by using @memset@.
--
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
word8N :: Int -> Word8 -> Builder ()
{-# INLINE word8N #-}
word8N x w8 = writeN x (\ mpa i -> setPrimArray mpa i x w8)

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

-- | Add "(..)" around builders when condition is met, otherwise add nothing.
--
-- This is useful when defining 'Print' instances.
parenWhen :: Bool -> Builder () -> Builder ()
{-# INLINE parenWhen #-}
parenWhen True b = paren b
parenWhen _    b = b

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
quotes b = encodePrim DOUBLE_QUOTE >> b >> encodePrim DOUBLE_QUOTE

-- | add @/'.../'@ to original builder.
squotes :: Builder () -> Builder ()
{-# INLINE squotes #-}
squotes b = encodePrim SINGLE_QUOTE >> b >> encodePrim SINGLE_QUOTE

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
intercalateVec sep f (V.Vec a s l)
    | l == 0 = return ()
    | otherwise = go s
  where
    !end = s + l - 1
    go !i | i == end = do
                f =<< A.indexArrM a i
          | otherwise = do
                f =<< A.indexArrM a i
                sep
                go (i+1)

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

