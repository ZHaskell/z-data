{-|
Module      : Z.Data.Decoder.Base
Description : Efficient deserialization/parse.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide internal data types for a simple resumable 'Decoder', which is suitable for binary protocol and simple textual protocol parsing. 'Decoder' extensively works on on 'V.Bytes', which is same to 'T.Text' representation.

-}

module Z.Data.Decoder.Base
  ( -- * Decoder types
    DecodeError
  , Decoder(..), fail'
    -- * Running a parser
  , decode, decode', decodeIO, decodeList, match
  --, runAndKeepTrack, match
    -- * Basic parsers
  , ensureN, endOfInput, atEnd
    -- * Primitive decoders
  , decodePrim, BE(..), LE(..)
  , decodePrimLE, decodePrimBE
  {-
    -- * More parsers
  , scan, scanChunks, peekMaybe, peek, satisfy, satisfyWith
  , anyWord8, word8, char8, anyChar8, anyCharUTF8, charUTF8, char7, anyChar7
  , skipWord8, endOfLine, skip, skipWhile, skipSpaces
  , take, takeN, takeTill, takeWhile, takeWhile1, takeRemaining, takeUTF8,  bytes, bytesCI
  , text
    -- * Error reporting
  , fail', failWithInput, unsafeLiftIO
    -- * Specialized primitive parser
  , decodeWord  , decodeWord64, decodeWord32, decodeWord16, decodeWord8
  , decodeInt   , decodeInt64 , decodeInt32 , decodeInt16 , decodeInt8 , decodeDouble, decodeFloat
  , decodeWordLE  , decodeWord64LE , decodeWord32LE , decodeWord16LE
  , decodeIntLE   , decodeInt64LE , decodeInt32LE , decodeInt16LE , decodeDoubleLE , decodeFloatLE
  , decodeWordBE  , decodeWord64BE , decodeWord32BE , decodeWord16BE
  , decodeIntBE   , decodeInt64BE , decodeInt32BE , decodeInt16BE , decodeDoubleBE , decodeFloatBE
  -}
  ) where

import           Control.Applicative
import           Control.Exception                  (Exception(..), assert)
import           Control.Monad
import           Control.Monad.Primitive
import qualified Control.Monad.Fail                 as Fail
import qualified Data.CaseInsensitive               as CI
import           Data.Int
import           Data.Word
import           Data.Bits                          ((.&.))
import           GHC.IO
import           GHC.Exts                           (State#, runRW#, unsafeCoerce#)
import           Prelude                            hiding (take, takeWhile, decodeFloat)
import           Z.Data.Ref
import           Z.Data.ASCII
import           Z.Data.Array                       as A
import qualified Z.Data.Text                        as T
import qualified Z.Data.Text.Base                   as T
import qualified Z.Data.Text.UTF8Codec              as T
import qualified Z.Data.Vector.Base                 as V
import qualified Z.Data.Vector.Extra                as V
import qualified Z.Data.Vector.Search               as V
import           Z.Data.Utils.Unaligned
import           Z.Monad.STE

-- | Type for decode error
data DecodeError = DecodeError
    { errorMsg :: {-# UNPACK #-} !T.Text,
      errorDecodeBuffer :: {-# UNPACK #-} !(PrimArray Word8, Int, Int)
        -- ^ current buffer state: buffer array, next offset and end offset
    } deriving (Show)
 
instance Exception DecodeError

-- | Decoder state
--
data DecodeBuffer s = DecodeBuffer
  { bufferRef :: {-# UNPACK #-} !(UnliftedRef s (PrimArray Word8))     
    -- ^ get the buffer content
  , offsetRef :: {-# UNPACK #-} !(OffsetRef s)                        
    -- ^ reading and end offset reference
  , nextChunk :: STE s DecodeError V.Bytes
    -- ^ called when buffer boundary is hit, provide with a error message, 
    -- a want size, or nothing means current decoder wants all rest of the chunks
  }

newtype OffsetRef s = OffsetRef (MutablePrimArray s Int)

newOffsetRef
    :: PrimMonad m
    => Int
    -> Int 
    -> m (OffsetRef (PrimState m))
{-# INLINE newOffsetRef #-}
newOffsetRef start end = do
    mpa <- newArray 2
    writeArray mpa 0 start
    writeArray mpa 1 end
    return (OffsetRef mpa)

writeOffsetRef :: PrimMonad m => OffsetRef (PrimState m) -> Int -> Int -> m ()
{-# INLINE writeOffsetRef #-}
writeOffsetRef (OffsetRef mpa) s l = do
    writeArray mpa 0 s
    writeArray mpa 1 l

readOffsetRef :: PrimMonad m => OffsetRef (PrimState m) -> m (Int, Int)
{-# INLINE readOffsetRef #-}
readOffsetRef (OffsetRef mpa) = do
    off <- readArray mpa 0
    end <- readArray mpa 1
    return (off, end)

-- | Save a decode buffer snapshot to a 'V.Bytes'.
--
decodeBufferSnapshot :: PrimMonad m => DecodeBuffer (PrimState m) -> m V.Bytes
{-# INLINE decodeBufferSnapshot #-}
decodeBufferSnapshot DecodeBuffer{..} = do
    buf <- readUnliftedRef bufferRef
    (off, end) <- readOffsetRef offsetRef
    return $! V.fromArray buf off (end - off)

-- | Restore the decode buffer state from a 'V.Bytes'.
--
decodeBufferRestore :: PrimMonad m => DecodeBuffer (PrimState m) -> V.Bytes -> m ()
{-# INLINE decodeBufferRestore #-}
decodeBufferRestore DecodeBuffer{..} (V.Slice buf s l) = do
    writeUnliftedRef bufferRef buf
    writeOffsetRef offsetRef s (s+l)

peekOffsetRef :: PrimMonad m => OffsetRef (PrimState m) -> m Int
{-# INLINE peekOffsetRef #-}
peekOffsetRef (OffsetRef mpa) = readArray mpa 0

pokeOffsetRef :: PrimMonad m => OffsetRef (PrimState m) -> Int -> m ()
{-# INLINE pokeOffsetRef #-}
pokeOffsetRef (OffsetRef mpa) off = writeArray mpa 0 off

--------------------------------------------------------------------------------

-- | Draw at least N bytes otherwise report "not enough bytes" error.
--
-- We update the buffer state no matter how many new bytes are drawn(even if not enough).
drawChunkAtLeast 
    :: Int          -- ^ want size
    -> T.Text       -- ^ error message if not enough bytes
    -> Decoder (PrimArray Word8, Int, Int)
{-# INLINE drawChunkAtLeast #-}
drawChunkAtLeast wantSiz message = Decoder $ \ db@DecodeBuffer{..} -> do
    (off, end) <- readOffsetRef offsetRef
    if (off + wantSiz > end) 
    then do
        bs <- decodeBufferSnapshot db
        bs' <- loopReadBytes (wantSiz - V.length bs) [bs]
        case bs' of
            (V.Slice arr s l) -> do
                decodeBufferRestore db bs'
                if (l < wantSiz) 
                then throwSTE (DecodeError (msg <> ": not enough bytes") (arr, s, s+l))
                else return (arr, s, s+l)
    else do
        buf <- readBufferRef bufferRef
        return (buf, off, end)
  where
    loopReadBytes !wantSiz bss = 
        if (wantSiz <= 0) 
        then return $! V.concat (reverse bss)
        else do
            bs <- nextChunk
            -- take empty as EOF
            if (V.null bs) 
            then return $! V.concat (reverse bss)
            else loopReadBytes (wantSiz - V.length bs) (bs:bss)

-- | Advance current buffer offset with diff.
advanceOffset :: Int -> Decoder ()
{-# INLINE advanceOffset #-}
advanceOffset x = Decoder $ \ DecodeBuffer{..} -> do
    off <- peekOffsetRef
    pokeOffsetRef offsetRef (off+x)

-- | Draw a new chunk if current chunk reach its end(off == end).
--
-- A new chunk is drawn and buffer state is update no matter the new chunk is empty or not.
drawChunkIfEndReached :: Decoder (PrimArray Word8, Int, Int)
{-# INLINE drawChunkIfEndReached #-}
drawChunkIfEndReached (OffsetRef mpa) = Decoder $ \ DecodeBuffer{..} -> do
    (off, end) <- readOffsetRef offsetRef
    when (off == end) (decodeBufferRestore =<< nextChunk)
    buf <- readBufferRef bufferRef
    (off', end') <- readOffsetRef offsetRef
    return (buf, off', end')


-- | @Decoder@ is a monad to help compose buffer decoding functions to parse binary or textual input.
--
newtype Decoder a = Decoder (forall s. DecodeBuffer s -> STE s DecodeError a)

instance Functor Decoder where
    {-# INLINE fmap #-}
    fmap f (Decoder b) = Decoder (\ buf -> fmap f (b buf))
    {-# INLINE (<$) #-}
    a <$ (Decoder b) = Decoder (\ buf -> b buf >> pure a)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure x = Decoder (\ _ -> pure x)
    {-# INLINE (<*>) #-}
    (Decoder f) <*> (Decoder g) = Decoder (\ buf -> do
        f' <- f buf
        g' <- g buf
        pure (f' g'))

instance Monad Decoder where
    {-# INLINE (>>=) #-}
    (Decoder b) >>= f = Decoder (\ buf -> do
        x <- b buf
        let Decoder b' = f x
        b' buf)

instance Fail.MonadFail Decoder where
    fail = fail' . T.pack
    {-# INLINE fail #-}

instance MonadPlus Decoder where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Alternative Decoder where
    empty = fail' "Z.Data.Decoder.Base(Alternative).empty"
    {-# INLINE empty #-}
    f <|> g = do
        r <- try f
        case r of Right r' -> return r'
                  _ -> g
    {-# INLINE (<|>) #-}

-- | 'T.Text' version of 'fail'.
fail' :: T.Text -> Decoder a
{-# INLINE fail' #-}
fail' msg = Decoder (\ DecodeBuffer{..} -> do
    buf <- readUnliftedRef bufferRef
    (off, end) <- readOffsetRef offsetRef
    throwSTE (DecodeError msg (buf, off, end)))

-- | Parse the complete input, without resupplying
decode :: Decoder a -> V.Bytes -> Either DecodeError a
{-# INLINABLE decode #-}
decode p inp = case decode' p inp of
    Right (_, r) -> Right r
    Left err -> Left err

-- | Parse the complete input, without resupplying, return any bytes left.
decode' :: Decoder a -> V.Bytes -> Either DecodeError (V.Bytes, a)
{-# INLINE decode' #-}
decode' (Decoder p) (V.Slice arr s l) = runSTE $ do
    bufRef <- newUnliftedRef arr
    offRef <- newOffsetRef s (s+l)
    -- just supply empty chunk after initial chunk
    r <- p (DecodeBuffer bufRef offRef (return V.empty))
    (off, end) <- readOffsetRef offRef
    return (V.Slice arr off (end - off), r)

-- | Parse the complete input list, without resupplying, return the rest bytes list.
--
-- Decoders in "Z.Data.Decoder" will take 'V.empty' as EOF, so please make sure there are no 'V.empty's
-- mixed into the chunk list.
decodeList :: forall a. Decoder a -> [V.Bytes] -> Either DecodeError ([V.Bytes], a)
{-# INLINABLE decodeList #-}
decodeList (Decoder p) ((V.Slice buf off len):bufs) = runSTE $ do
    bufferListRef <- newRef bufs
    bufRef <- newUnliftedRef buf
    offRef <- newOffsetRef off (off+len)
    let decodeBuffer = (DecodeBuffer bufRef offRef (popBufferFromList bufferListRef))
    r <- p decodeBuffer
    bs <- decodeBufferSnapshot decodeBuffer
    bufferLeft <- readRef bufferListRef
    return (if V.null bs then bufferLeft else bs:bufferLeft, r)
  where
    popBufferFromList bufferListRef = do
        bufsLeft <- readRef bufferListRef
        case bufsLeft of
            (v:bufs) -> do
                writeRef bufferListRef bufs
                return v
            _ -> return V.empty
decodeList p _ = decodeList p [V.empty]

-- | Decode within IO monad with a reading function.
decodeIO :: Decoder a
         -> V.Bytes        -- ^ initial chunk
         -> IO V.Bytes     -- ^ reading function, return empty bytes to indicate EOF
         -> IO (V.Bytes, a)           -- ^ DecodeError will be thrown if any 
{-# INLINE decodeIO #-}
decodeIO (Decoder p) (V.Slice arr s l) nextChunk = do
    bufRef <- newUnliftedRef arr
    offRef <- newOffsetRef s (s+l)
    -- decodeIO run STE with concrete RealWorld state token
    -- which is perfectly safe
    let decodeBuffer = DecodeBuffer bufRef offRef (ioToSTE nextChunk)
    !r <- steToIO (p decodeBuffer)
    !bs <- decodeBufferSnapshot decodeBuffer
    return (bs, r)


-- | Draw next chunk, put it to decode buffer, and save it to a chunk list reference.
recordNextChunk :: DecodeBuffer s -> Ref s [V.Bytes] -> STE s DecodeError V.Bytes
recordNextChunk db@DecodeBuffer{..} scannedChunksRef = do
    bs <- nextChunk
    modifyRef scannedChunksRef (bs :)
    decodeBufferRestore db bs
    return bs

-- | Return both the result of a parse and the portion of the input
-- that was consumed while it was being parsed.
--
match :: Decoder a -> Decoder (V.Bytes, a)
{-# INLINE match #-}
match (Decoder p) = Decoder $ \ db@DecodeBuffer{..} -> do
    bs <- decodeBufferSnapshot db
    scannedChunksRef <- newRef [bs]
    !r <- p (DecodeBuffer bufferRef offsetRef 
        (recordNextChunk db scannedChunksRef))
    (off, end) <- readOffsetRef offsetRef
    scannedChunks <- readRef scannedChunksRef
    let !matched = V.dropR (end - off) (V.concat (reverse scannedChunks))
    return (matched, r)

-- | Run a Decoder, catch decode errors and restore decode buffer state when if failed.
--
try :: Decoder a -> Decoder (Either DecodeError a)
{-# INLINE try #-}
try (Decoder p) = Decoder ( \ db@DecodeBuffer{..} -> do
    bs <- decodeBufferSnapshot db
    scannedChunksRef <- newRef [bs]
    catchSTE
        (Right <$> p (DecodeBuffer bufferRef offsetRef
            (recordNextChunk db scannedChunksRef)))
        (handleError db scannedChunksRef))
  where
    handleError db scannedChunksRef = \ e -> do
        scannedChunks <- readRef scannedChunksRef
        decodeBufferRestore db (V.concat (reverse scannedChunks))
        return (Left e)

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
--
-- Since this parser is used in many other parsers, an extra error param is provide
-- to attach custom error info.
readN
    :: Int
    -> T.Text
    -> (forall s. PrimArray Word8 -> Int -> Int -> STE s DecodeError a)
    -> Decoder a
{-# INLINE readN #-}
readN siz msg f = Decoder $ \ db@DecodeBuffer{..} -> do
    (buf, off, end) <- drawChunkAtLeast siz msg
    r <- f buf off end
    advanceOffset siz
    return r

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes. Fail if not 'atEnd'.
endOfInput :: Decoder ()
{-# INLINE endOfInput #-}
endOfInput = do
    (buf, off, end) <- drawChunkIfEndReached 
    unless (end == off) $
        fail' "Z.Data.Decoder.endOfInput: EOF not reached"

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
atEnd :: Decoder Bool
{-# INLINE atEnd #-}
atEnd = do
    (_, off, end) <- drawChunkIfEndReached 
    return $! end == off

-- | Decode a primitive type in host byte order.
decodePrim :: forall a. (Unaligned a) => Decoder a
{-# INLINE decodePrim #-}
decodePrim = do
    (buf, off, _) <- drawChunkAtLeast n "Z.Data.Decoder.decodePrim"
    advanceOffset n
    return $! indexOff buf off
  where
    n = getUnalignedSize (unalignedSize @a)

#define DECODE_HOST(f, type) \
    f :: Decoder type; {-# INLINE f #-}; f = decodePrim; \
    -- ^ Decode type in host endian order.

DECODE_HOST(decodeWord  , Word   )
DECODE_HOST(decodeWord64, Word64 )
DECODE_HOST(decodeWord32, Word32 )
DECODE_HOST(decodeWord16, Word16 )
DECODE_HOST(decodeWord8 , Word8  )
DECODE_HOST(decodeInt   , Int    )
DECODE_HOST(decodeInt64 , Int64  )
DECODE_HOST(decodeInt32 , Int32  )
DECODE_HOST(decodeInt16 , Int16  )
DECODE_HOST(decodeInt8  , Int8   )
DECODE_HOST(decodeDouble, Double )
DECODE_HOST(decodeFloat , Float  )

-- | Decode a primitive type in little endian.
decodePrimLE :: forall a. (Unaligned (LE a)) => Decoder a
{-# INLINE decodePrimLE #-}
decodePrimLE = do
    (buf, off, _) <- drawChunkAtLeast n "Z.Data.Decoder.decodePrimLE"
    advanceOffset n
    return $! getLE (indexOff buf off)
  where
    n = getUnalignedSize (unalignedSize @(LE a))

#define DECODE_LE(f, type) \
    f :: Decoder type; {-# INLINE f #-}; f = decodePrimLE; \
    -- ^ Decode type in little endian order.

DECODE_LE(decodeWordLE  , Word   )
DECODE_LE(decodeWord64LE, Word64 )
DECODE_LE(decodeWord32LE, Word32 )
DECODE_LE(decodeWord16LE, Word16 )
DECODE_LE(decodeIntLE   , Int    )
DECODE_LE(decodeInt64LE , Int64  )
DECODE_LE(decodeInt32LE , Int32  )
DECODE_LE(decodeInt16LE , Int16  )
DECODE_LE(decodeDoubleLE, Double )
DECODE_LE(decodeFloatLE , Float  )

-- | Decode a primitive type in big endian.
decodePrimBE :: forall a. (Unaligned (BE a)) => Decoder a
{-# INLINE decodePrimBE #-}
decodePrimBE = do
    (buf, off, _) <- drawChunkAtLeast n "Z.Data.Decoder.decodePrimBE"
    advanceOffset n
    return $! getBE (indexOff buf off)
  where
    n = getUnalignedSize (unalignedSize @(BE a))

#define DECODE_BE(f, type) \
    f :: Decoder type; {-# INLINE f #-}; f = decodePrimBE; \
    -- ^ Decode type in big endian order.

DECODE_BE(decodeWordBE  , Word   )
DECODE_BE(decodeWord64BE, Word64 )
DECODE_BE(decodeWord32BE, Word32 )
DECODE_BE(decodeWord16BE, Word16 )
DECODE_BE(decodeIntBE   , Int    )
DECODE_BE(decodeInt64BE , Int64  )
DECODE_BE(decodeInt32BE , Int32  )
DECODE_BE(decodeInt16BE , Int16  )
DECODE_BE(decodeDoubleBE, Double )
DECODE_BE(decodeFloatBE , Float  )

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each byte of the input until one
-- returns 'Nothing', if a 'Nothing' is not reached before input ends,
-- all consumed chunks will be returned.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
scan :: s -> (s -> Word8 -> Maybe s) -> Decoder (V.Bytes, s)
{-# INLINE scan #-}
scan s0 bytescan = scanChunks s0 chunkscan
  where
    chunkscan s0 buf off end =
        let loop !s !i
                | i < end =
                    case bytescan s (indexArray buf i) of
                        Just s' -> loop s' (i+1)
                        _       -> Right (i, s)
                | otherwise = Left s
        in loop s0 off

-- | Similar to 'scan', but working on 'V.Bytes' chunks, The predicate
-- consumes a 'V.Bytes' chunk and transforms a state argument,
-- and each transformed state is passed to successive invocations of
-- the predicate on each chunk of the input until one chunk got splited to
-- @Right (V.Bytes, V.Bytes)@, if a 'Right' result is not reached before input ends,
-- all consumed chunks will be returned.
--
scanChunks 
    :: forall s. s
    -> (s -> PrimArray Word8 -> Int -> Int -> Either s (Int, s)) 
        -- ^ scanner function receive previous state, buffer array, start offset, end offset,
        -- it should return a new state if end is not reached, or final state with next read offset.
    -> Decoder (V.Bytes, s)
{-# INLINE scanChunks #-}
scanChunks s0 consume = do
    (buf, off, end) <- drawChunkIfEndReached
    if (off == end)
    then return (V.empty, s0)
    else case consume s buf off end of
        -- common case, we found break point inside current chunk
        Right (off', s') -> do
            a



        Left s' ->
            Decoder $ \ db -> do 
                scannedChunksRef <- newRef []
                !s1 <- scanLoop db scannedChunksRef s0
                scannedChunks <- readRef scannedChunksRef
                let !bs = V.concat (reverse scannedChunks)
                return (bs, s1))
  where 
    scanLoop :: DecodeBuffer st -> Ref st [V.Bytes] -> s -> STE st DecodeError s
    scanLoop db@DecodeBuffer{..} scannedChunksRef !s = do
        buf <- readUnliftedRef bufferRef 
        (off, end) <- readOffsetRef offsetRef
        case consume s buf off end of
            Right (off', s') -> do
                -- push latest chunk and stop scan
                let !bs = V.Slice buf off (off' - off)
                modifyRef scannedChunksRef (bs :)
                -- update offset
                pokeOffsetRef offsetRef off'
                return s'
            Left s' -> do
                let !bs = V.Slice buf off (end - off)
                modifyRef scannedChunksRef (bs :)
                -- draw next chunk
                decodeBufferRestore db =<< nextChunk
                (off', end') <- readOffsetRef offsetRef
                -- if EOF is not reached then continue scan
                when (off' /= end') (scanLoop db scannedChunksRef s')

--------------------------------------------------------------------------------

-- | Match any byte, to perform lookahead. Returns 'Nothing' if end of
-- input has been reached. Does not consume any input.
--
peekMaybe :: Decoder (Maybe Word8)
{-# INLINE peekMaybe #-}
peekMaybe = do
    (buf, off, end) <- drawChunkIfEndReached
    if (end == off) 
    then return Nothing
    else return . Just $! A.indexArray buf off

-- | Match any byte, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
--
peek :: Decoder Word8
{-# INLINE peek #-}
peek = do
    (buf, off, end) <- drawChunkIfEndReached
    if (end == off) 
    then fail' "Z.Data.Decoder.peek: not enough bytes" 
    else return $! A.indexArray buf off

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
--
satisfy :: (Word8 -> Bool) -> Decoder Word8
{-# INLINE satisfy #-}
satisfy p = do
    (buf, off, end) <- drawChunkAtLeast 1 "Z.Data.Decoder.satisfy"
    let !w = A.readArray arr off
    if (p w)
    then advanceOffset 1 >> return w
    else fail' "Z.Data.Decoder.Base.satisfy: unsatisfied byte"

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
--
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Decoder a
{-# INLINE satisfyWith #-}
satisfyWith f p = do
    (buf, off, end) <- drawChunkAtLeast 1 "Z.Data.Decoder.satisfyWith"
    let !w = A.readArray arr off
        !a = f w
    if (p a)
    then advanceOffset 1 >> return a
    else fail' "Z.Data.Decoder.Base.satisfyWith: unsatisfied byte"

-- | Match a specific byte.
--
word8 :: Word8 -> Decoder ()
{-# INLINE word8 #-}
word8 w = do
    (buf, off, end) <- drawChunkAtLeast 1 "Z.Data.Decoder.statisfy"
    if (w == A.readArray buf off)
    then advanceOffset 1
    else fail' "Z.Data.Decoder.Base.word8: unsatisfied byte"

-- | Return a byte, this is an alias to @decodePrim @Word8@.
--
anyWord8 :: Decoder Word8
{-# INLINE anyWord8 #-}
anyWord8 = decodePrim

-- | Match a specific 8bit char.
--
char8 :: Char -> Decoder ()
{-# INLINE char8 #-}
char8 = word8 . c2w

-- | Match a specific 7bit char.
--
char7 :: Char -> Decoder ()
{-# INLINE char7 #-}
char7 chr = word8 (c2w chr .&. 0x7F)

-- | Match a specific UTF8 char.
--
charUTF8 :: Char -> Decoder ()
{-# INLINE charUTF8 #-}
charUTF8 = text . T.singleton

-- | Take a byte and return as a 8bit char.
--
anyChar8 :: Decoder Char
{-# INLINE anyChar8 #-}
anyChar8 = do
    w <- anyWord8
    return $! w2c w

-- | Take a byte and return as a 7bit char, fail if exceeds @0x7F@.
--
anyChar7 :: Decoder Char
{-# INLINE anyChar7 #-}
anyChar7 = do
    w <- satisfy (<= 0x7f)
    return $! w2c w

-- | Decode next few bytes as an UTF8 char.
--
-- Don't use this method as UTF8 decoder, it's slower than 'T.validate'.
anyCharUTF8 :: Decoder Char
{-# INLINE anyCharUTF8 #-}
anyCharUTF8 = do
    (buf, off, end) <- drawChunkIfEndReached
    if (off == end) 
    then fail' "Z.Data.Decoder.anyCharUTF8: EOF reached"
    else do
        let charLen = T.decodeCharLen buf off
        (buf, off, end) <- drawChunkAtLeast charLen "Z.Data.Decoder.anyCharUTF8"
        case T.validateMaybe (V.Slice buf off charLen) of
            Just t -> do
                advanceOffset charLen
                return $! T.head t
            _ -> fail' "Z.Data.Decoder.anyCharUTF8: invalid UTF8 bytes"

-- | Match either a single newline byte @\'\\n\'@, or a carriage
-- return followed by a newline byte @\"\\r\\n\"@.
endOfLine :: Decoder ()
{-# INLINE endOfLine #-}
endOfLine = do
    w <- decodePrim :: Decoder Word8
    case w of
        10 -> return ()
        13 -> do
            w' <- decodePrim :: Decoder Word8
            unless (w' == 10) . fail' $ 
                "Z.Data.Decoder.Base.endOfLine: mismatch byte, expected 10, meet " <> T.toText w
        _  -> fail' $
            "Z.Data.Decoder.Base.endOfLine: mismatch byte, expected 10 or 13, meet " <> T.toText w

--------------------------------------------------------------------------------

-- | 'skip' N bytes.
--
skip :: Int -> Decoder ()
{-# INLINE skip #-}
skip n0 = do
    (buf, off, end) <- drawChunkIfEndReached 
    if (off == end)
    then fail' "Z.Data.Decoder.skip: EOF reached"
    else 
        if (off + n0 > end)
        then Decoder scanLoop (off + n0 - end)
        else advanceOffset n0
  where
    scanLoop db@DecodeBuffer{..} !n = do
        bs <- nextChunk
        if (V.null bs) 
        then do
            decodeBufferRestore db bs

        else do
            let !rest = V.dropWhile p bs
            if (V.null rest)
            then scanLoop db
            else decodeBufferRestore db rest
    
-- | Skip a byte.
--
skipWord8 :: Decoder ()
{-# INLINE skipWord8 #-}
skipWord8 = do
    (buf, off, end) <- drawChunkIfEndReached
    if (end == off) 
    then fail' (DecodeError "Z.Data.Decoder.skipWord8: not enough bytes")
    else advanceOffset 1

-- | Skip past input for as long as the predicate returns 'True'.
--
skipWhile :: (Word8 -> Bool) -> Decoder ()
{-# INLINE skipWhile #-}
skipWhile p = do
    (buf, off, end) <- drawChunkIfEndReached 
    let !rest = V.dropWhile p (V.Slice buf off (end-off))
    if (V.null rest)
    then Decoder scanLoop
    else decodeBufferRestore db rest
  where
    scanLoop db@DecodeBuffer{..} = do
        bs <- nextChunk
        if (V.null rest) 
        then decodeBufferRestore db bs
        else do
            let !rest = V.dropWhile p bs
            if (V.null rest)
            then scanLoop db
            else decodeBufferRestore db rest

-- | Skip over white space using 'isSpace'.
--
skipSpaces :: Decoder ()
{-# INLINE skipSpaces #-}
skipSpaces = skipWhile isSpace

-- | Take N bytes.
take :: Int -> Decoder V.Bytes
{-# INLINE take #-}
take n = do
    (buf, off, end) <- drawChunkAtLeast n "Z.Data.Decoder.Base.take"
    return $! V.Slice buf off end

-- | Consume input as long as the predicate returns 'False' or reach the end of input,
-- and return the consumed input.
--
takeTill :: (Word8 -> Bool) -> Decoder V.Bytes
{-# INLINABLE takeTill #-}
takeTill p = do
    (buf, off, end) <- drawChunkIfEndReached db
    if (off /= end)
    then do
        let chunk = V.Slice buf off (end - off)
        let idx = V.findIndex p chunk
        if idx < V.length chunk
        -- common path
        then do
            advanceOffset idx
            return $! V.unsafeTake idx chunk
        else Decoder ( \ db -> do
            scannedChunksRef <- newRef [chunk]
            scanLoop db scannedChunksRef
            scannedChunks <- readRef scannedChunksRef
            return $! V.concat (reverse scannedChunks))
    else return V.empty
  where 
    scanLoop :: DecodeBuffer st -> Ref st [V.Bytes] -> STE st DecodeError ()
    scanLoop db@DecodeBuffer{..} scannedChunksRef = do
        chunk@(V.Slice _ off len) <- nextChunk
        decodeBufferRestore db chunk
        when (len /= 0) $ do
            case V.findIndex p chunk of
                idx | idx < len -> do
                        -- push consumed chunk and stop scan
                        modifyRef scannedChunksRef (V.unsafeTake idx chunk :)
                        -- update offset
                        pokeOffsetRef offsetRef (off + idx)
                     | otherwise -> do
                        -- push whole
                        modifyRef scannedChunksRef (chunk :)
                        -- and scan again
                        scanLoop db scannedChunksRef

-- | Consume input as long as the predicate returns 'True' or reach the end of input,
-- and return the consumed input.
--
takeWhile :: (Word8 -> Bool) -> Decoder V.Bytes
{-# INLINE takeWhile #-}
takeWhile p = takeTill (not . p)

-- | Similar to 'takeWhile', but requires the predicate to succeed on at least one byte
-- of input: it will fail if reach the end of input.
--
takeWhileNonEmpty :: (Word8 -> Bool) -> Decoder V.Bytes
{-# INLINE takeWhileNonEmpty #-}
takeWhileNonEmpty p = do
    bs <- takeWhile p
    if V.null bs
    then fail' "Z.Data.Decoder.Base.takeWhile1: no satisfied byte"
    else return bs

-- | Similar to 'takeWhile', but requires the predicate to succeed on next N bytes
-- of input.
--
takeWhileN :: (Word8 -> Bool) -> Int -> Decoder V.Bytes
{-# INLINE takeWhileN #-}
takeWhileN p n = do
    bs@(V.Slice arr s l) <- take n
    !s' <- loop arr s l
    if s' == l
    then return bs
    else do
        advanceOffset (s'-s)
        fail' "Z.Data.Decoder.takeWhileN: no satisfied byte"
  where 
    loop arr s l
        | s == l = return s
        | otherwise = if p (A.indexArray arr s) then loop arr (s+1) l else s

-- | Take all the remaining input chunks and return as 'V.Bytes'.
takeRemaining :: Decoder V.Bytes
{-# INLINE takeRemaining #-}
takeRemaining = do
    (buf, off, end) <- drawChunkIfEndReached
    if (off /= end)
    then Decoder ( \ db -> do
        scannedChunksRef <- newRef [(V.Slice buf off (end - off))]
        loopTake db scannedChunksRef
        scannedChunks <- readRef scannedChunksRef
        return $! V.concat (reverse scannedChunks))
    else return V.empty
  where 
    loopTake db@DecodeBuffer{..} scannedChunksRef = do
        bs <- nextChunk
        if (V.null bs)
        then decodeBufferRestore db bs
        else do
            modifyRef scannedChunksRef (bs :)
            loopTake db scannedChunksRef

        

-- | Take N bytes and validate as UTF8, failed if not UTF8 encoded.
takeUTF8 :: Int -> Decoder T.Text
{-# INLINE takeUTF8 #-}
takeUTF8 n = do
    bs <- take n
    case T.validateMaybe bs of Just t -> pure t
                               _ -> fail' "Z.Data.Decoder.Base.takeUTF8: illegal UTF8 bytes"


-- | @bytes s@ parses a sequence of bytes that identically match @s@.
--
bytes :: V.Bytes -> Decoder ()
{-# INLINE bytes #-}
bytes bs = do
    let n = V.length bs
    bs' <- take n
    when (bs /= bs') $
        fail' ("Z.Data.Decoder.bytes: different bytes meet, target " <> T.toText bs)


-- | Same as 'bytes' but ignoring ASCII case.
bytesCI :: V.Bytes -> Decoder ()
{-# INLINE bytesCI #-}
bytesCI bs = do
    let n = V.length bs
    bs' <- take n
    when (bs_ci /= CI.foldCase bs') $
        fail' ("Z.Data.Decoder.bytesCI: different CI bytes meet, target " <> T.toText bs)
  where
    bs_ci = CI.foldCase bs

-- | @text s@ parses a sequence of UTF8 bytes that identically match @s@.
--
text :: T.Text -> Decoder ()
{-# INLINE text #-}
text (T.Text bs) = bytes bs
