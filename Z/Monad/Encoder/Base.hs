{-|
Module      : Z.Data.Encoder.Base
Description : Efficient serialization/format.
Copyright   : (c) Dong Han, 2017-2019
              (c) Tao He, 2018-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

A 'Encoder' records a buffer writing function, which can be 'mappend' in O(1) via composition.

  * When building a short strict 'Bytes' with 'build'\/'buildWith', we double the buffer
    each time buffer is full.

  * When building a large strict @[Bytes]@ with 'buildChunks'\/'buildChunksWith',
    we insert a new chunk when buffer is full.

-}

module Z.Data.Encoder.Base
  ( -- * Encoder type
    Encoder(..)
  , append
  , EncodeBuffer(..)
  , freezeBuffer
  , freezeBufferRef
   -- * Running a builder
  , build
  , buildWith
  , buildAndRun
  , buildAndRunWith
  , buildAndRunWithBuffer
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
  -- * Encoder helpers
  , paren, parenWhen, curly, square, angle, quotes, squotes, colon, comma, intercalateVector, intercalateList
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
import           Control.Exception                  (throwIO, Exception(..))
import           Data.Bits                          (unsafeShiftL, unsafeShiftR, (.&.))
import           Data.Word
import           Data.Int
import           GHC.Exts                           hiding (build)
import           GHC.ST
import           GHC.IO                           
import           GHC.Stack
import           Z.Data.ASCII
import qualified Z.Data.Text.Base                   as T
import qualified Z.Data.Text.UTF8Codec              as T
import qualified Z.Data.Vector.Base                 as V
import           Z.Data.Array
import           Z.Data.Ref
import           Z.Data.Utils.Unaligned
import           Prelude                            hiding (encodeFloat)
import           System.IO.Unsafe
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

-- | Encoder state
--
data EncodeBuffer s = EncodeBuffer
  { bufferRef :: {-# UNPACK #-} !(UnliftedRef s (MutablePrimArray s Word8))        -- ^ get the buffer content
  , offsetRef :: {-# UNPACK #-} !(PrimRef s Int)        -- ^ writing offset reference
  , onBufferFull :: Either V.Bytes Int -> ST s ()       -- ^ called when buffer boundary is hit,
                                                        -- provide either a chunk to be inserted without enough space
                                                        -- or a want size
  }

-- | Freeze buffer from a buffer reference and return a 'V.Bytes'.
--
-- Note the mutable buffer array will be shrinked with 'shrinkMutableArray', which may not
-- able to be reused.
freezeBufferRef :: PrimMonad m => UnliftedRef (PrimState m) (MutablePrimArray (PrimState m) Word8) -> Int -> m V.Bytes
{-# INLINE freezeBufferRef #-}
freezeBufferRef bufRef off = do
    buf <- readUnliftedRef bufRef
    siz <- sizeofMutableArray buf
    when (off < siz) (shrinkMutableArray buf off)
    !arr <- unsafeFreezeArray buf
    return (V.PrimVector arr 0 off)

-- | Freeze buffer and return a 'V.Bytes'.
--
-- Note the mutable buffer array will be shrinked with 'shrinkMutableArray', which may not
-- able to be reused.
freezeBuffer :: PrimMonad m => MutablePrimArray (PrimState m) Word8 -> Int -> m V.Bytes
{-# INLINE freezeBuffer #-}
freezeBuffer buf off = do
    siz <- sizeofMutableArray buf
    when (off < siz) (shrinkMutableArray buf off)
    !arr <- unsafeFreezeArray buf
    return (V.PrimVector arr 0 off)

-- | @Encoder@ is a monad to help compose buffer writing functions to generate binary or textual output.
--
-- Notes on 'IsString' instance: @Encoder ()@'s 'IsString' instance use 'stringModifiedUTF8',
-- which is different from 'stringUTF8' in that it DOES NOT PROVIDE UTF8 GUARANTEES! :
--
-- * @\\NUL@ will be written as @\\xC0 \\x80@.
-- * @\\xD800@ ~ @\\xDFFF@ will be encoded in three bytes as normal UTF-8 codepoints.
--
newtype Encoder a = Encoder (forall s. EncodeBuffer s -> ST s a)

instance Show (Encoder a) where
    show = show . build

instance Functor Encoder where
    {-# INLINE fmap #-}
    fmap f (Encoder b) = Encoder (\ buf -> fmap f (b buf))
    {-# INLINE (<$) #-}
    a <$ (Encoder b) = Encoder (\ buf -> b buf >> pure a)

instance Applicative Encoder where
    {-# INLINE pure #-}
    pure x = Encoder (\ _ -> pure x)
    {-# INLINE (<*>) #-}
    (Encoder f) <*> (Encoder g) = Encoder (\ buf -> do
        f' <- f buf
        g' <- g buf
        pure (f' g'))
    {-# INLINE (*>) #-}
    (*>) = append

instance Monad Encoder where
    {-# INLINE (>>=) #-}
    (Encoder b) >>= f = Encoder (\ buf -> do
        x <- b buf
        let Encoder b' = f x
        b' buf)
    {-# INLINE (>>) #-}
    (>>) = (*>)

instance Semigroup (Encoder ()) where
    (<>) = append
    {-# INLINE (<>) #-}

instance Monoid (Encoder ()) where
    mempty = pure ()
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}
    mconcat = foldr append (pure ())
    {-# INLINE mconcat #-}

-- | This instance simple write literals' bytes into buffer,
-- which is different from 'stringUTF8' in that it DOES NOT PROVIDE UTF8 GUARANTEES! :
instance (a ~ ()) => IsString (Encoder a) where
    {-# INLINE fromString #-}
    fromString = stringModifiedUTF8

instance Arbitrary (Encoder ()) where
    arbitrary = bytes <$> arbitrary
    shrink b = (bytes . V.pack) <$> shrink (V.unpack (build b))

instance CoArbitrary (Encoder ()) where
    coarbitrary = coarbitrary . build

-- | Encode string with modified UTF-8 encoding, will be rewritten to a memcpy if possible.
stringModifiedUTF8 :: String -> Encoder ()
{-# INLINE CONLIKE [0] stringModifiedUTF8 #-}
{-# RULES
    "stringModifiedUTF8/packAddrModified" forall addr . stringModifiedUTF8 (unpackCString# addr) = packAddrModified addr
  #-}
{-# RULES
    "stringModifiedUTF8/packAddrModified" forall addr . stringModifiedUTF8 (unpackCStringUtf8# addr) = packAddrModified addr
  #-}
stringModifiedUTF8 = mapM_ charModifiedUTF8

-- | Turn 'Char' into 'Encoder' with Modified UTF8 encoding
--
-- @\\NUL@ is encoded as two bytes @C0 80@ , @\\xD800@ ~ @\\xDFFF@ is encoded as a three bytes normal UTF-8 codepoint.
charModifiedUTF8 :: Char -> Encoder ()
{-# INLINE charModifiedUTF8 #-}
charModifiedUTF8 chr = do
    ensureN 4 (\ mba i -> T.encodeCharModifiedUTF8 mba i chr)

packAddrModified :: Addr# -> Encoder ()
{-# INLINE packAddrModified #-}
packAddrModified addr0# = copy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    copy addr# = do
        writeN len (\ mba i -> copyPtrToMutablePrimArray mba i (Ptr addr#) len)

append :: Encoder a -> Encoder b -> Encoder b
{-# INLINE [1] append #-}
append (Encoder f) (Encoder g) = Encoder (\ buf -> f buf >> g buf)

--------------------------------------------------------------------------------

-- | Write a 'V.Bytes'.
bytes :: V.Bytes -> Encoder ()
{-# INLINE bytes #-}
bytes v@(V.PrimVector arr s l) = Encoder (\ EncodeBuffer{..} -> do
    buf <- readUnliftedRef bufferRef
    off <- readPrimRef offsetRef
    siz <- sizeofMutableArray buf
    let off' = off + l
    if off' <= siz
    then do
        copyArray buf off arr s l
        writePrimRef offsetRef off'
    else onBufferFull (Left v))

--------------------------------------------------------------------------------

-- | Shortcut to 'buildWith' 'V.defaultInitSize'.
build :: Encoder a -> V.Bytes
{-# INLINE build #-}
build = buildWith V.defaultInitSize

-- | Build some bytes and validate if it's UTF8 bytes.
buildText :: HasCallStack => Encoder a -> T.Text
{-# INLINE buildText #-}
buildText = T.validate . buildWith V.defaultInitSize

-- | Build some bytes assuming it's UTF8 encoding.
--
-- Be carefully use this function because you could constrcut illegal 'T.Text' values.
-- Check 'Z.Data.Text.ShowT' for UTF8 encoding builders. This functions is intended to
-- be used in debug only.
unsafeBuildText :: Encoder a -> T.Text
{-# INLINE unsafeBuildText #-}
unsafeBuildText = T.Text . buildWith V.defaultInitSize

-- | Run Encoder with doubling buffer strategy, which is suitable
-- for building short bytes.
buildWith :: Int -> Encoder a -> V.Bytes
{-# INLINABLE buildWith #-}
buildWith initSiz (Encoder b) = runST $ do
    bufRef <- newUnliftedRef =<< newArray initSiz
    offRef <- newPrimRef 0
    let buffer = EncodeBuffer bufRef offRef (doubleBuffer bufRef offRef)
    _ <- b buffer
    freezeBufferRef bufRef =<< readPrimRef offRef
  where
    doubleBuffer bufRef offRef = \ x -> case x of
        Left (V.PrimVector arr s l) -> do
            buf <- readUnliftedRef bufRef
            off <- readPrimRef offRef
            !siz <- sizeofMutableArray buf
            let !siz' = max (off + l `unsafeShiftL` 1) (siz `unsafeShiftL` 1)
            buf' <- resizeMutableArray buf siz'   -- grow buffer
            copyArray buf' off arr s l
            writeUnliftedRef bufRef buf'
            writePrimRef offRef (off + l)
        Right wantSiz -> do
            buf <- readUnliftedRef bufRef
            off <- readPrimRef offRef
            !siz <- sizeofMutableArray buf
            let !siz' = max (off + wantSiz `unsafeShiftL` 1)
                            (siz `unsafeShiftL` 1)
            buf' <- resizeMutableArray buf siz'   -- grow buffer
            writeUnliftedRef bufRef buf'

-- | Shortcut to 'buildAndRunWith' 'V.defaultChunkSize'.
buildAndRun :: Encoder a -> (V.Bytes -> IO ()) -> IO ()
{-# INLINE buildAndRun #-}
buildAndRun = buildAndRunWith V.defaultChunkSize

-- | Run Encoder with action callback, which is suitable
-- for processing them in a streaming ways.
--
-- Note this functions DO NO REUSE buffers, you can safely save the result chunks for later use.
buildAndRunWith :: Int -> Encoder a -> (V.Bytes -> IO ()) -> IO ()
{-# INLINABLE buildAndRunWith #-}
buildAndRunWith chunkSiz (Encoder b) action = do
    bufRef <- newUnliftedRef =<< newArray chunkSiz
    offRef <- newPrimRef 0
    let buffer = EncodeBuffer bufRef offRef (switchBuffer bufRef offRef)
    _ <- stToIO (b buffer)
    off <- readPrimRef offRef
    when (off /= 0) (do
        !v <- freezeBufferRef bufRef off
        action v)
  where
    switchBuffer bufRef offRef = \ x -> case x of
        Left v@(V.PrimVector arr s l) -> ioToST $ do
            off <- readPrimRef offRef
            if off == 0
            then do
              -- buffer is OK, but inserted chunk is too large to fit
              action v
            else do
                !v' <- freezeBufferRef bufRef off
                buf' <- newArray chunkSiz   -- new buffer
                writeUnliftedRef bufRef buf'
                if l < chunkSiz `unsafeShiftR` 1
                then do
                    -- copy inserted chunk to new buffer
                    copyArray buf' 0 arr s l
                    writePrimRef offRef l
                    action v'
                else do
                    -- prepend inserted chunk to list
                    writePrimRef offRef 0
                    action v'
                    action v

        Right wantSiz -> ioToST $ do
            off <- readPrimRef offRef
            when (off /= 0) (do
                !v <- freezeBufferRef bufRef off
                action v)
            let !siz' = max chunkSiz wantSiz
            buf' <- newArray siz'   -- new buffer
            writeUnliftedRef bufRef buf'
            writePrimRef offRef 0

-- | Run Encoder with action callback and a buffer.
--
-- Note this functions REUSE buffers, you should NOT use the result chunk after action callback!
buildAndRunWithBuffer
    :: HasCallStack 
    => (MutablePrimArray RealWorld Word8, Int) -> Encoder a -> (V.Bytes -> IO ()) -> IO ()
{-# INLINABLE buildAndRunWithBuffer #-}
buildAndRunWithBuffer (initBuf, chunkSiz) (Encoder b) action = do
    bufRef <- newUnliftedRef initBuf
    offRef <- newPrimRef 0
    let buffer = EncodeBuffer bufRef offRef (switchBuffer offRef)
    _ <- stToIO (b buffer)
    off <- readPrimRef offRef
    when (off /= 0) (action =<< freezeBuffer initBuf off)
  where
    switchBuffer offRef = \ x -> case x of
        Left v@(V.PrimVector arr s l) -> ioToST $ do
            off <- readPrimRef offRef
            if off == 0
              -- buffer is OK, but inserted chunk is too large to fit
            then action v
            else do
                action =<< freezeBuffer initBuf off
                if l < chunkSiz `unsafeShiftR` 1
                then do
                    -- copy inserted chunk to new buffer
                    writePrimRef offRef l
                    copyArray initBuf 0 arr s l
                else do
                    -- directly action on chunk to be inserted
                    writePrimRef offRef 0
                    action v

        Right wantSiz -> ioToST $ do
            off <- readPrimRef offRef
            when (off /= 0) (action =<< freezeBuffer initBuf off)
            writePrimRef offRef 0
            when (wantSiz > chunkSiz) (throwIO (BufferTooSmall callStack))

data BufferTooSmall = BufferTooSmall CallStack deriving Show
instance Exception BufferTooSmall

-- | Shortcut to 'buildChunksWith' 'V.defaultChunkSize'.
buildChunks :: Encoder a -> [V.Bytes]
{-# INLINE buildChunks #-}
buildChunks = buildChunksWith V.defaultChunkSize

-- | Run Encoder with inserting chunk strategy, which is suitable
-- for building a list of bytes chunks.
--
-- Note the building process is strict, all list chunks will stay in memory during building.
buildChunksWith :: Int -> Encoder a -> [V.Bytes]
{-# INLINABLE buildChunksWith #-}
buildChunksWith chunkSiz (Encoder b) = runST $ do
    bufRef <- newUnliftedRef =<< newArray chunkSiz
    offRef <- newPrimRef 0
    bufListRef <- newRef []
    let buffer = EncodeBuffer bufRef offRef (switchBuffer bufListRef bufRef offRef)
    _ <- b buffer
    off <- readPrimRef offRef
    when (off /= 0) (do
        !v <- freezeBufferRef bufRef off
        modifyRef bufListRef (v:))
    reverse <$> readRef bufListRef
  where
    switchBuffer
        :: forall s. Ref s [V.Bytes] 
        -> UnliftedRef s (MutablePrimArray s Word8)
        -> PrimRef s Int
        -> Either V.Bytes Int -> ST s ()
    switchBuffer bufListRef bufRef offRef = \ x -> case x of
        Left v@(V.PrimVector arr s l) -> do
            off <- readPrimRef offRef
            if off == 0
            then do
              -- buffer is OK, but inserted chunk is too large to fit
              modifyRef bufListRef (v:)
            else do
                !v' <- freezeBufferRef bufRef off
                buf' <- newArray chunkSiz   -- new buffer
                writeUnliftedRef bufRef buf'
                if l < chunkSiz `unsafeShiftR` 1
                then do
                    -- copy inserted chunk to new buffer
                    copyArray buf' 0 arr s l
                    writePrimRef offRef l
                    modifyRef bufListRef (v':)
                else do
                    -- prepend inserted chunk to list
                    writePrimRef offRef 0
                    modifyRef bufListRef (\vs -> v:v':vs)

        Right wantSiz -> do
            off <- readPrimRef offRef
            when (off /= 0) (do
                !v <- freezeBufferRef bufRef off
                modifyRef bufListRef (v:))
            let !siz' = max chunkSiz wantSiz
            buf' <- newArray siz'   -- new buffer
            writeUnliftedRef bufRef buf'
            writePrimRef offRef 0

--------------------------------------------------------------------------------

ensureN :: Int  -- ^ size bound
       -> (forall s. MutablePrimArray s Word8 -> Int -> ST s Int)   -- ^ the writer which return a new offset
                                                                    -- for next write
       -> Encoder ()
{-# INLINE [1] ensureN #-}
ensureN !n f = Encoder (\ EncodeBuffer{..} -> do
    !buf <- readUnliftedRef bufferRef
    !off <- readPrimRef offsetRef
    !siz <- sizeofMutableArray buf
    when ((n + off) > siz) (onBufferFull (Right n))
    !buf' <- readUnliftedRef bufferRef
    !off' <- readPrimRef offsetRef
    writePrimRef offsetRef =<< f buf' off')

writeN :: Int  -- ^ size bound
       -> (forall s. MutablePrimArray s Word8 -> Int -> ST s ())  -- ^ the writer should write exactly N bytes
       -> Encoder ()
{-# INLINE writeN #-}
writeN n f = ensureN n (\ mba i -> f mba i >> (return $! i + n))

{-# RULES "ensureN/merge" forall
    n1 (f1 :: forall s. MutablePrimArray s Word8 -> Int -> ST s Int)
    n2 (f2 :: forall s. MutablePrimArray s Word8 -> Int -> ST s Int).
    append (ensureN n1 f1) (ensureN n2 f2) = ensureN (n1 + n2) (\ mba i -> f1 mba i >>= f2 mba) #-}

-- | Write a primitive type in host byte order.
--
-- @
-- > encodePrim (256 :: Word16, BE 256 :: BE Word16)
-- > [0,1,1,0]
-- @
encodePrim :: forall a. Unaligned a => a -> Encoder ()
{-# INLINE encodePrim #-}
encodePrim x = do
    writeN n (\ mpa i -> writeOff mpa i x)
  where
    n = getUnalignedSize (unalignedSize @a)

#define ENCODE_HOST(f, type) \
    f :: type -> Encoder (); {-# INLINE f #-}; f = encodePrim; \
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
encodePrimLE :: forall a. Unaligned (LE a) => a -> Encoder ()
{-# INLINE encodePrimLE #-}
encodePrimLE = encodePrim . LE

#define ENCODE_LE(f, type) \
    f :: type -> Encoder (); {-# INLINE f #-}; f = encodePrimLE; \
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
encodePrimBE :: forall a. Unaligned (BE a) => a -> Encoder ()
{-# INLINE encodePrimBE #-}
encodePrimBE = encodePrim . BE

#define ENCODE_BE(f, type) \
    f :: type -> Encoder (); {-# INLINE f #-}; f = encodePrimBE; \
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

-- | Turn 'String' into 'Encoder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
--
-- This is different from writing string literals builders via @OverloadedStrings@, because string literals
-- do not provide UTF8 guarantees.
--
-- This function will be rewritten into a memcpy if possible, (running a fast UTF-8 validation
-- at runtime first).
stringUTF8 :: String -> Encoder ()
{-# INLINE CONLIKE [0] stringUTF8 #-}
{-# RULES
    "stringUTF8/packASCIIAddr" forall addr . stringUTF8 (unpackCString# addr) = packASCIIAddr addr
  #-}
{-# RULES
    "stringUTF8/packUTF8Addr" forall addr . stringUTF8 (unpackCString# addr) = packUTF8Addr addr
  #-}
stringUTF8 = mapM_ charUTF8

packASCIIAddr :: Addr# -> Encoder ()
{-# INLINE packASCIIAddr #-}
packASCIIAddr addr0# = copy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    copy addr# = do
        writeN len (\ mba i -> copyPtrToMutablePrimArray mba i (Ptr addr#) len)

packUTF8Addr :: Addr# -> Encoder ()
{-# INLINABLE packUTF8Addr #-}
packUTF8Addr addr0# = validateAndCopy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#
    valid = unsafeDupablePerformIO $ T.c_utf8_validate_addr addr0# len
    validateAndCopy addr#
        | valid == 0 = mapM_ charUTF8 (unpackCString# addr#)
        | otherwise = do
            writeN len (\ mba i -> copyPtrToMutablePrimArray mba i (Ptr addr#) len)

-- | Turn 'Char' into 'Encoder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
charUTF8 :: Char -> Encoder ()
{-# INLINE charUTF8 #-}
charUTF8 chr = do
    ensureN 4 (\ mba i -> T.encodeChar mba i chr)

-- | Turn 'String' into 'Encoder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
string7 :: String -> Encoder ()
{-# INLINE string7 #-}
string7 = mapM_ char7

-- | Turn 'Char' into 'Encoder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
char7 :: Char -> Encoder ()
{-# INLINE char7 #-}
char7 chr = writeN 1 (\ mpa i -> writeOff mpa i (c2w chr .&. 0x7F))

-- | Turn 'Word8' into 'Encoder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
word7 :: Word8 -> Encoder ()
{-# INLINE word7 #-}
word7 w = writeN 1 (\ mpa i -> writeOff mpa i (w .&. 0x7F))

-- | Turn 'String' into 'Encoder' with ASCII8 encoding
--
-- Codepoints beyond @'\xFF'@ will be chopped.
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
string8 :: String -> Encoder ()
{-# INLINE string8 #-}
string8 = mapM_ char8

-- | Turn 'Char' into 'Encoder' with ASCII8 encoding
--
-- Codepoints beyond @'\xFF'@ will be chopped.
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
char8 :: Char -> Encoder ()
{-# INLINE char8 #-}
char8 chr = writeN 1 (\ mpa i -> writeOff mpa i (c2w chr))

-- | Turn 'Word8' into 'Encoder' with ASCII8 encoding, (alias to 'encodePrim').
--
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
word8 :: Word8 -> Encoder ()
{-# INLINE word8 #-}
word8 = encodePrim

-- | Faster version of @replicateM x . word8@ by using @memset@.
--
-- Note, this encoding is NOT compatible with UTF8 encoding, i.e. bytes written
-- by this builder may not be legal UTF8 encoding bytes.
word8N :: Int -> Word8 -> Encoder ()
{-# INLINE word8N #-}
word8N x w8 = writeN x (\ mpa i -> setArray mpa i x w8)

-- | Write UTF8 encoded 'Text' using 'Encoder'.
--
-- Note, if you're trying to write string literals builders,
-- please open 'OverloadedStrings' and use 'Encoder's 'IsString' instance,
-- it will be rewritten into a memcpy.
text :: T.Text -> Encoder ()
{-# INLINE text #-}
text (T.Text bs) = bytes bs

--------------------------------------------------------------------------------


-- | add @(...)@ to original builder.
paren :: Encoder () -> Encoder ()
{-# INLINE paren #-}
paren b = encodePrim PAREN_LEFT >> b >> encodePrim PAREN_RIGHT

-- | Add "(..)" around builders when condition is met, otherwise add nothing.
--
-- This is useful when defining 'Print' instances.
parenWhen :: Bool -> Encoder () -> Encoder ()
{-# INLINE parenWhen #-}
parenWhen True b = paren b
parenWhen _    b = b

-- | add @{...}@ to original builder.
curly :: Encoder () -> Encoder ()
{-# INLINE curly #-}
curly b = encodePrim CURLY_LEFT >> b >> encodePrim CURLY_RIGHT

-- | add @[...]@ to original builder.
square :: Encoder () -> Encoder ()
{-# INLINE square #-}
square b = encodePrim SQUARE_LEFT >> b >> encodePrim SQUARE_RIGHT

-- | add @/<.../>@ to original builder.
angle :: Encoder () -> Encoder ()
{-# INLINE angle #-}
angle b = encodePrim ANGLE_LEFT >> b >> encodePrim ANGLE_RIGHT

-- | add @/".../"@ to original builder.
quotes :: Encoder () -> Encoder ()
{-# INLINE quotes #-}
quotes b = encodePrim DOUBLE_QUOTE >> b >> encodePrim DOUBLE_QUOTE

-- | add @/'.../'@ to original builder.
squotes :: Encoder () -> Encoder ()
{-# INLINE squotes #-}
squotes b = encodePrim SINGLE_QUOTE >> b >> encodePrim SINGLE_QUOTE

-- | write an ASCII @:@
colon :: Encoder ()
{-# INLINE colon #-}
colon = encodePrim COLON

-- | write an ASCII @,@
comma :: Encoder ()
{-# INLINE comma #-}
comma = encodePrim COMMA

-- | Use separator to connect a vector of builders.
--
-- @
-- import Z.Data.Encoder as B
-- import Z.Data.Text    as T
-- import Z.Data.Vector  as V
--
-- > T.validate . B.build $ B.intercalateVector "," B.int (V.pack [1,2,3,4] :: V.PrimVector Int)
-- "1,2,3,4"
-- @
intercalateVector :: (V.Vectors v a)
                  => Encoder ()           -- ^ the seperator
                  -> (a -> Encoder ())    -- ^ value formatter
                  -> v a                  -- ^ value vector
                  ->  Encoder ()
{-# INLINE intercalateVector #-}
intercalateVector sep f (V.Slice a s l)
    | l == 0 = return ()
    | otherwise = go s
  where
    !end = s + l - 1
    go !i | i == end = do
                f =<< indexArrayM a i
          | otherwise = do
                f =<< indexArrayM a i
                sep
                go (i+1)

-- | Use separator to connect list of builders.
--
-- @
-- import Z.Data.Encoder as B
-- import Z.Data.Text    as T
-- import Z.Data.Vector  as V
--
-- T.validate . B.build $ B.intercalateList "," B.int ([1,2,3,4] :: [Int])
-- "1,2,3,4"
-- @
intercalateList :: Encoder ()           -- ^ the seperator
                -> (a -> Encoder ())    -- ^ value formatter
                -> [a]                  -- ^ value list
                -> Encoder ()
{-# INLINE intercalateList #-}
intercalateList s f xs = go xs
  where
    go [] = pure ()
    go [x] = f x
    go (x:xs') = f x >> s >> go xs'

