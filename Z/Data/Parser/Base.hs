{-|
Module      : Z.Data.Parser.Base
Description : Efficient deserialization/parse.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide internal data types for a simple resumable 'Parser', which is suitable for binary protocol and simple textual protocol parsing. 'Parser' extensively works on on 'V.Bytes', which is same to 'T.Text' representation.

-}

module Z.Data.Parser.Base
  ( -- * Parser types
    Result(..)
  , ParseError
  , ParseStep
  , Parser(..)
  , (<?>)
    -- * Running a parser
  , parse, parse', parseChunk, parseChunkList, ParseChunks, parseChunks, finishParsing
  , runAndKeepTrack, match
    -- * Basic parsers
  , ensureN, endOfInput, currentChunk, atEnd
    -- * Primitive decoders
  , decodePrim, BE(..), LE(..)
  , decodePrimLE, decodePrimBE
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
  ) where

import           Control.Applicative
import           Control.Exception                  (assert)
import           Control.Monad
import           Control.Monad.Primitive
import qualified Control.Monad.Fail                 as Fail
import qualified Data.CaseInsensitive               as CI
import qualified Data.Primitive.PrimArray           as A
import           Data.Int
import           Data.Word
import           Data.Bits                          ((.&.))
import           GHC.Types
import           GHC.Exts                           (State#, runRW#, unsafeCoerce#)
import           Prelude                            hiding (take, takeWhile, decodeFloat)
import           Z.Data.Array.Unaligned
import           Z.Data.ASCII
import qualified Z.Data.Text                        as T
import qualified Z.Data.Text.Base                   as T
import qualified Z.Data.Text.UTF8Codec              as T
import qualified Z.Data.Vector.Base                 as V
import qualified Z.Data.Vector.Extra                as V

-- | Simple parsing result, that represent respectively:
--
-- * Success: the remaining unparsed data and the parsed value
--
-- * Failure: the remaining unparsed data and the error message
--
-- * Partial: that need for more input data, supply empty bytes to indicate 'endOfInput'
--
data Result e r
    = Success r !V.Bytes
    | Failure e !V.Bytes
    | Partial (ParseStep e r)

-- | A parse step consumes 'V.Bytes' and produce 'Result'.
type ParseStep e r = V.Bytes -> Result e r

-- | Type alias for error message
type ParseError = [T.Text]

instance Functor (Result e) where
    fmap f (Success a s)   = Success (f a) s
    fmap f (Partial k)     = Partial (fmap f . k)
    fmap _ (Failure e v)   = Failure e v

instance (Show e, Show a) => Show (Result e a) where
    show (Success a _)    = "Success " ++ show a
    show (Partial _)      = "Partial _"
    show (Failure errs _) = "Failure: " ++ show errs

-- | Simple CPSed parser
--
-- A parser takes a failure continuation, and a success one, while the success continuation is
-- usually composed by 'Monad' instance, the failure one is more like a reader part, which can
-- be modified via '<?>'. If you build parsers from ground, a pattern like this can be used:
--
--  @
--    xxParser = do
--      ensureN errMsg ...            -- make sure we have some bytes
--      Parser $ \ kf k s inp ->      -- fail continuation, success continuation, state token and input
--        ...
--        ... kf errMsg (if input not OK)
--        ... k s ... (if we get something useful for next parser)
--  @
newtype Parser a = Parser {
        runParser :: forall r . (ParseError -> ParseStep ParseError r)
                  -> (State# ParserState -> a -> ParseStep ParseError r)
                  -> State# ParserState -> ParseStep ParseError r
    }

-- | State token tag used in `Parser`
data ParserState

-- It seems eta-expand all params to ensure parsers are saturated is helpful
instance Functor Parser where
    fmap = fmapParser
    {-# INLINE fmap #-}
    a <$ Parser pb = Parser (\ kf k s inp -> pb kf (\ s' _ -> k s' a) s inp)
    {-# INLINE (<$) #-}

fmapParser :: (a -> b) -> Parser a -> Parser b
{-# INLINE fmapParser #-}
fmapParser f (Parser pa) = Parser (\ kf k s inp -> pa kf (\ s' -> k s' . f) s inp)

instance Applicative Parser where
    pure x = Parser (\ _ k s inp -> k s x inp)
    {-# INLINE pure #-}
    (<*>) = apParser
    {-# INLINE (<*>) #-}
    Parser pa *> Parser pb = Parser (\ kf k s inp -> pa kf (\ s' _ -> pb kf k s') s inp)
    {-# INLINE (*>) #-}
    Parser pa <* Parser pb = Parser (\ kf k s inp -> pa kf (\ s' x -> pb kf (\ s'' _ -> k s'' x) s') s inp)
    {-# INLINE (<*) #-}

apParser :: Parser (a -> b) -> Parser a -> Parser b
{-# INLINE apParser #-}
apParser (Parser pf) (Parser pa) = Parser (\ kf k s inp -> pf kf (\ s' f -> pa kf (\ s'' -> k s'' . f) s') s inp)

instance Monad Parser where
    return = pure
    {-# INLINE return #-}
    Parser pa >>= f = Parser (\ kf k s inp -> pa kf (\ s' a -> runParser (f a) kf k s') s inp)
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}

instance PrimMonad Parser where
    type PrimState Parser = ParserState
    {-# INLINE primitive #-}
    primitive io = Parser (\ _ k st inp ->
        let !(# st', r #) = io st
        in k st' r inp)

{-# RULES "replicateM/Parser" forall n (x :: Parser a). V.replicateM n x = V.replicatePM n x #-}
{-# RULES "traverse/Parser" forall (f :: a -> Parser b). V.traverse f = V.traverseWithIndexPM (const f) #-}
{-# RULES "traverseWithIndex/Parser" forall (f :: Int -> a -> Parser b). V.traverseWithIndex f = V.traverseWithIndexPM f #-}

-- | Unsafely lifted an `IO` action into 'Parser'.
--
-- This is only for debugging purpose(logging, etc). Don't mix compuation from
-- realworld to parsing result, otherwise parsing is not deterministic.
unsafeLiftIO :: IO a -> Parser a
{-# INLINE unsafeLiftIO #-}
unsafeLiftIO (IO io) = Parser $ \ _ k st inp ->
    let !(# st', r #) = io (unsafeCoerce# st)
    in k (unsafeCoerce# st') r inp

instance Fail.MonadFail Parser where
    fail = fail' . T.pack
    {-# INLINE fail #-}

instance MonadPlus Parser where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Alternative Parser where
    empty = fail' "Z.Data.Parser.Base(Alternative).empty"
    {-# INLINE empty #-}
    f <|> g = do
        (r, consumed) <- runAndKeepTrack f
        case r of
            Success x inp   -> Parser (\ _ k s _ -> k s x inp)
            Failure _ _     -> Parser (\ kf k s _ -> runParser g kf k s consumed)
            _               -> error "Z.Data.Parser.Base: impossible"
    {-# INLINE (<|>) #-}

-- | 'T.Text' version of 'fail'.
fail' :: T.Text -> Parser a
{-# INLINE fail' #-}
fail' msg = Parser (\ kf _ _ inp -> kf [msg] inp)

-- | Similar to `fail'`, but can produce error message with current input chunk.
failWithInput :: (V.Bytes -> T.Text) -> Parser a
{-# INLINE failWithInput #-}
failWithInput f = Parser (\ kf _ _ inp -> kf [f inp] inp)

-- | Parse the complete input, without resupplying
parse' :: Parser a -> V.Bytes -> Either ParseError a
{-# INLINABLE parse' #-}
parse' p = snd . parse p

-- | Parse the complete input, without resupplying, return the rest bytes
parse :: Parser a -> V.Bytes -> (V.Bytes, Either ParseError a)
{-# INLINE parse #-}
parse (Parser p) = \ inp ->
    case (runRW# ( \ s -> unsafeCoerce# (p Failure (\ _ r -> Success r) (unsafeCoerce# s) inp))) of
        Success a rest    -> (rest, Right a)
        Failure errs rest -> (rest, Left errs)
        Partial f         -> finishParsing (f V.empty)

-- | Parse the complete input list, without resupplying, return the rest bytes list.
--
-- Parsers in "Z.Data.Parser" will take 'V.empty' as EOF, so please make sure there are no 'V.empty's
-- mixed into the chunk list.
parseChunkList :: Parser a -> [V.Bytes] -> ([V.Bytes], Either ParseError a)
{-# INLINABLE parseChunkList #-}
parseChunkList p (inp:inps) = go (parseChunk p inp) inps
    where
        go r is = case r of
            Partial f -> case is of
                (i:is') -> go (f i) is'
                _ -> let (rest, r') = finishParsing r
                     in (if V.null rest then [] else [rest], r')
            Success a rest    -> (if V.null rest then is else rest:is, Right a)
            Failure errs rest -> (if V.null rest then is else rest:is, Left errs)

-- | Parse an input chunk
parseChunk :: Parser a -> V.Bytes -> Result ParseError a
{-# INLINE parseChunk #-}
parseChunk (Parser p) = runRW# (\ s ->
    unsafeCoerce# (p Failure (\ _ r -> Success r) (unsafeCoerce# s)))

-- | Finish parsing and fetch result, feed empty bytes if it's 'Partial' result.
finishParsing :: Result ParseError a -> (V.Bytes, Either ParseError a)
{-# INLINABLE finishParsing #-}
finishParsing r = case r of
    Success a rest    -> (rest, Right a)
    Failure errs rest -> (rest, Left errs)
    Partial f         -> finishParsing (f V.empty)

-- | Type alias for a streaming parser, draw chunk from Monad m with a initial chunk,
-- return result in @Either err x@.
type ParseChunks m err x = m V.Bytes -> V.Bytes -> m (V.Bytes, Either err x)

-- | Run a chunk parser with an initial input string, and a monadic action
-- that can supply more input if needed.
--
parseChunks :: Monad m => (V.Bytes -> Result e a) -> ParseChunks m e a
{-# INLINE parseChunks #-}
parseChunks pc m inp = go (pc inp)
  where
    go r = case r of
        Partial f -> go . f =<< m
        Success a rest    -> pure (rest, Right a)
        Failure errs rest -> pure (rest, Left errs)

(<?>) :: T.Text -> Parser a -> Parser a
{-# INLINE (<?>) #-}
msg <?> (Parser p) = Parser (\ kf k s inp -> p (kf . (msg:)) k s inp)
infixr 0 <?>

-- | Run a parser and keep track of all the input chunks it consumes.
-- Once it's finished, return the final result (always 'Success' or 'Failure') and
-- all consumed chunks.
--
runAndKeepTrack :: Parser a -> Parser (Result ParseError a, V.Bytes)
{-# INLINE runAndKeepTrack #-}
runAndKeepTrack (Parser pa) = Parser $ \ _ k0 st0 inp ->
    let go !acc r k (st :: State# ParserState) = case r of
            Partial k'      -> Partial (\ inp' -> go (inp':acc) (k' inp') k st)
            Success _ inp' -> let consumed = V.concatR acc in consumed `seq` k st (r, consumed) inp'
            Failure _ inp' -> let consumed = V.concatR acc in consumed `seq` k st (r, consumed) inp'
        r0 = runRW# (\ s ->
                unsafeCoerce# (pa Failure (\ _ r -> Success r) (unsafeCoerce# s) inp))
    in go [inp] r0 k0 st0

-- | Return both the result of a parse and the portion of the input
-- that was consumed while it was being parsed.
match :: Parser a -> Parser (V.Bytes, a)
{-# INLINE match #-}
match p = do
    (r, consumed) <- runAndKeepTrack p
    Parser (\ _ k s _ ->
        case r of
            Success r' inp'  -> k s (consumed , r') inp'
            Failure err inp' -> Failure err inp'
            Partial _        -> error "Z.Data.Parser.Base.match: impossible")

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
--
-- Since this parser is used in many other parsers, an extra error param is provide
-- to attach custom error info.
ensureN :: Int -> T.Text -> Parser ()
{-# INLINE ensureN #-}
ensureN n0 err = Parser $ \ kf k s inp -> do
    let l = V.length inp
    if n0 <= l
    then k s () inp
    else Partial (ensureNPartial (n0-l) inp kf k s)
  where
    ensureNPartial :: forall r. Int -> V.PrimVector Word8 -> (ParseError -> ParseStep ParseError r)
                   -> (State# ParserState -> () -> ParseStep ParseError r)
                   -> State# ParserState -> ParseStep ParseError r
    ensureNPartial !l0 inp0 kf k s0 =
        let go acc !l s = \ inp -> do
                let l' = V.length inp
                if l' == 0
                then kf [err] (V.concatR (inp:acc))
                else do
                    if l <= l'
                    then let !inp' = V.concatR (inp:acc) in k s () inp'
                    else Partial (go (inp:acc) (l - l') s)
        in go [inp0] l0 s0

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
--
-- Since this parser is used in many other parsers, an extra error param is provide
-- to attach custom error info.
readN :: forall a. Int -> T.Text -> (V.Bytes -> a) -> Parser a
{-# INLINE readN #-}
readN n0 err f = Parser $ \ kf k s inp -> do
    let l = V.length inp
    if n0 <= l
    then let !r = f inp
             !inp' = V.unsafeDrop n0 inp
             in k s r inp'
    else Partial (readNPartial (n0-l) inp kf k s)
  where
    readNPartial :: forall r. Int -> V.PrimVector Word8 -> (ParseError -> ParseStep ParseError r)
                   -> (State# ParserState -> a -> ParseStep ParseError r)
                   -> State# ParserState -> ParseStep ParseError r
    readNPartial !l0 inp0 kf k s0 =
        let go acc !l s = \ inp -> do
                let l' = V.length inp
                if l' == 0
                then kf [err] (V.concatR (inp:acc))
                else do
                    if l <= l'
                    then let !inp' = V.concatR (inp:acc)
                             !r  = f inp'
                             !inp'' = V.unsafeDrop n0 inp'
                         in k s r inp''
                    else Partial (go (inp:acc) (l - l') s)
        in go [inp0] l0 s0

{- These rules are bascially what inliner do so no need to mess up with them
{-# RULES "readN/fmap"
    forall n f g e. fmapParser f (readN n e g)  = readN n e (f . g) #-}
{-# RULES "readN/merge"
    forall n1 n2 e1 e2 f1 f2. apParser (readN n1 e1 f1) (readN n2 e2 f2) = readN (n1 + n2) (T.concat [e1, ", ", e2]) (\ inp -> f1 inp $! f2 (V.unsafeDrop n1 inp)) #-}
-}

-- | Get current input chunk, draw new chunk if neccessary. 'V.null' means EOF.
--
-- Note this is different from 'takeRemaining', 'currentChunk' only return what's
-- left in current input chunk.
currentChunk :: Parser V.Bytes
{-# INLINE currentChunk #-}
currentChunk =  Parser $ \ _ k s inp ->
    if V.null inp
    then Partial (\ inp' -> k s inp' V.empty)
    else k s inp V.empty

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes. Fail if not 'atEnd'.
endOfInput :: Parser ()
{-# INLINE endOfInput #-}
endOfInput = Parser $ \ kf k s inp ->
    if V.null inp
    then Partial (\ inp' ->
        if (V.null inp')
        then k s () inp'
        else kf ["Z.Data.Parser.Base.endOfInput: end not reached yet"] inp')
    else kf ["Z.Data.Parser.Base.endOfInput: end not reached yet"] inp

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
atEnd :: Parser Bool
{-# INLINE atEnd #-}
atEnd = Parser $ \ _ k s inp ->
    if V.null inp
    then Partial (\ inp' -> k s (V.null inp') inp')
    else k s False inp

-- | Decode a primitive type in host byte order.
decodePrim :: forall a. (Unaligned a) => Parser a
{-# INLINE decodePrim #-}
decodePrim = do
    readN n "Z.Data.Parser.Base.decodePrim: not enough bytes" (\ (V.PrimVector ba i _) -> indexPrimWord8ArrayAs ba i)
  where
    n = getUnalignedSize (unalignedSize @a)

#define DECODE_HOST(f, type) \
    f :: Parser type; {-# INLINE f #-}; f = decodePrim; \
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
decodePrimLE :: forall a. (Unaligned (LE a)) => Parser a
{-# INLINE decodePrimLE #-}
decodePrimLE = do
    readN n "Z.Data.Parser.Base.decodePrimLE: not enough bytes" (\ (V.PrimVector ba i _) -> getLE (indexPrimWord8ArrayAs ba i))
  where
    n = getUnalignedSize (unalignedSize @(LE a))

#define DECODE_LE(f, type) \
    f :: Parser type; {-# INLINE f #-}; f = decodePrimLE; \
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
decodePrimBE :: forall a. (Unaligned (BE a)) => Parser a
{-# INLINE decodePrimBE #-}
decodePrimBE = do
    readN n "Z.Data.Parser.Base.decodePrimBE: not enough bytes" (\ (V.PrimVector ba i _) -> getBE (indexPrimWord8ArrayAs ba i))
  where
    n = getUnalignedSize (unalignedSize @(BE a))

#define DECODE_BE(f, type) \
    f :: Parser type; {-# INLINE f #-}; f = decodePrimBE; \
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
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
scan :: s -> (s -> Word8 -> Maybe s) -> Parser (V.Bytes, s)
{-# INLINE scan #-}
scan s0 f = scanChunks s0 f'
  where
    f' s0' (V.PrimVector arr off l) =
        let !end = off + l
            go !st !i
                | i < end = do
                    let !w = A.indexPrimArray arr i
                    case f st w of
                        Just st' -> go st' (i+1)
                        _        ->
                            let !len1 = i - off
                                !len2 = end - off
                            in Right (V.PrimVector arr off len1, V.PrimVector arr i len2, st)
                | otherwise = Left st
        in go s0' off

-- | Similar to 'scan', but working on 'V.Bytes' chunks, The predicate
-- consumes a 'V.Bytes' chunk and transforms a state argument,
-- and each transformed state is passed to successive invocations of
-- the predicate on each chunk of the input until one chunk got splited to
-- @Right (V.Bytes, V.Bytes)@ or the input ends.
--
-- Note the fields of result triple will not be forced by 'scanChunks', you may need to add `seq` or strict annotation to
-- avoid thunks and unintentional references to buffer.
--
scanChunks :: forall s. s -> (s -> V.Bytes -> Either s (V.Bytes, V.Bytes, s)) -> Parser (V.Bytes, s)
{-# INLINE scanChunks #-}
scanChunks s0 consume = Parser (\ _ k st inp ->
    case consume s0 inp of
        Right (want, rest, s') -> k st (want, s') rest
        Left s' -> Partial (scanChunksPartial s' k st inp))
  where
    -- we want to inline consume if possible
    {-# INLINE scanChunksPartial #-}
    scanChunksPartial :: forall r. s -> (State# ParserState -> (V.PrimVector Word8, s) -> ParseStep ParseError r)
                      -> State# ParserState -> V.PrimVector Word8 -> ParseStep ParseError r
    scanChunksPartial s0' k st0 inp0 =
        let go s acc st = \ inp ->
                if V.null inp
                then k st (V.concatR acc, s) inp
                else case consume s inp of
                        Left s' -> do
                            let acc' = inp : acc
                            Partial (go s' acc' st)
                        Right (want,rest,s') ->
                            let !r = V.concatR (want:acc) in k st (r, s') rest
        in go s0' [inp0] st0

--------------------------------------------------------------------------------

-- | Match any byte, to perform lookahead. Returns 'Nothing' if end of
-- input has been reached. Does not consume any input.
--
peekMaybe :: Parser (Maybe Word8)
{-# INLINE peekMaybe #-}
peekMaybe =
    Parser $ \ _ k s inp ->
        if V.null inp
        then Partial (\ inp' -> k s (if V.null inp'
            then Nothing
            else Just (V.unsafeHead inp')) inp')
        else k s (Just (V.unsafeHead inp)) inp

-- | Match any byte, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
--
peek :: Parser Word8
{-# INLINE peek #-}
peek =
    Parser $ \ kf k s inp ->
        if V.null inp
        then Partial (\ inp' ->
            if V.null inp'
            then kf ["Z.Data.Parser.Base.peek: not enough bytes"] inp'
            else k s (V.unsafeHead inp') inp')
        else k s (V.unsafeHead inp) inp

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
--
satisfy :: (Word8 -> Bool) -> Parser Word8
{-# INLINE satisfy #-}
satisfy p = do
    ensureN 1 "Z.Data.Parser.Base.satisfy: not enough bytes"
    Parser $ \ kf k s inp ->
        let w = V.unsafeHead inp
        in if p w
            then k s w (V.unsafeTail inp)
            else kf [ "Z.Data.Parser.Base.satisfy: unsatisfied bytes " <> T.toText (V.take 8 inp) ]
                    (V.unsafeTail inp)

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
--
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
{-# INLINE satisfyWith #-}
satisfyWith f p = do
    ensureN 1 "Z.Data.Parser.Base.satisfyWith: not enough bytes"
    Parser $ \ kf k s inp ->
        let a = f (V.unsafeHead inp)
        in if p a
            then k s a (V.unsafeTail inp)
            else kf ["Z.Data.Parser.Base.satisfyWith: unsatisfied byte"] (V.unsafeTail inp)

-- | Match a specific byte.
--
word8 :: Word8 -> Parser ()
{-# INLINE word8 #-}
word8 w' = do
    ensureN 1 "Z.Data.Parser.Base.word8: not enough bytes"
    Parser (\ kf k s inp ->
        let w = V.unsafeHead inp
        in if w == w'
            then k s () (V.unsafeTail inp)
            else kf [ T.concat [
                 "Z.Data.Parser.Base.word8: mismatch byte, expected "
                , T.toText w'
                , ", meet "
                , T.toText w
                , " at "
                , T.toText (V.take 8 inp)
                ] ] inp)

-- | Return a byte, this is an alias to @decodePrim @Word8@.
--
anyWord8 :: Parser Word8
{-# INLINE anyWord8 #-}
anyWord8 = decodePrim

-- | Match a specific 8bit char.
--
char8 :: Char -> Parser ()
{-# INLINE char8 #-}
char8 = word8 . c2w

-- | Match a specific 7bit char.
--
char7 :: Char -> Parser ()
{-# INLINE char7 #-}
char7 chr = word8 (c2w chr .&. 0x7F)

-- | Match a specific UTF8 char.
--
charUTF8 :: Char -> Parser ()
{-# INLINE charUTF8 #-}
charUTF8 = text . T.singleton

-- | Take a byte and return as a 8bit char.
--
anyChar8 :: Parser Char
{-# INLINE anyChar8 #-}
anyChar8 = do
    w <- anyWord8
    return $! w2c w

-- | Take a byte and return as a 7bit char, fail if exceeds @0x7F@.
--
anyChar7 :: Parser Char
{-# INLINE anyChar7 #-}
anyChar7 = do
    w <- satisfy (<= 0x7f)
    return $! w2c w

-- | Decode next few bytes as an UTF8 char.
--
-- Don't use this method as UTF8 decoder, it's slower than 'T.validate'.
anyCharUTF8 :: Parser Char
{-# INLINE anyCharUTF8 #-}
anyCharUTF8 = do
    r <- Parser $ \ kf k st inp@(V.PrimVector arr s l) -> do
        if l > 0
        then
            let l' = T.decodeCharLen arr s
            in if l' > l
            then k st (Left l') inp
            else do
                case T.validateMaybe (V.unsafeTake l' inp) of
                    Just t -> k st (Right $! T.head t) $! V.unsafeDrop l' inp
                    _ -> kf ["Z.Data.Parser.Base.anyCharUTF8: invalid UTF8 bytes"] inp
        else k st (Left 1) inp
    case r of
        Left d -> do
            ensureN d "Z.Data.Parser.Base.anyCharUTF8: not enough bytes"
            anyCharUTF8
        Right c -> return c

-- | Match either a single newline byte @\'\\n\'@, or a carriage
-- return followed by a newline byte @\"\\r\\n\"@.
endOfLine :: Parser ()
{-# INLINE endOfLine #-}
endOfLine = do
    w <- decodePrim :: Parser Word8
    case w of
        10 -> return ()
        13 -> word8 10

        _  -> Parser (\ kf _ _ inp -> kf [
            T.concat [
             "Z.Data.Parser.Base.endOfLine: mismatch byte, expected 10 or 13, meet "
            , T.toText w
            , " at "
            , T.toText (V.cons w (V.take 8 inp))
            ] ] inp)

--------------------------------------------------------------------------------

-- | 'skip' N bytes.
--
skip :: Int -> Parser ()
{-# INLINE skip #-}
skip n = assert (n > 0) $
    Parser (\ kf k s inp ->
        let l = V.length inp
        in if l >= n
            then k s () $! V.unsafeDrop n inp
            else Partial (skipPartial (n-l) kf k s))
  where
    skipPartial :: Int -> (ParseError -> ParseStep ParseError r)
                -> (State# ParserState -> () -> ParseStep ParseError r)
                -> State# ParserState -> ParseStep ParseError r
    skipPartial n0 kf k s0 =
        let go !n' s = \ inp ->
                let l = V.length inp
                in if l >= n'
                    then k s () $! V.unsafeDrop n' inp
                    else if l == 0
                        then kf ["Z.Data.Parser.Base.skip: not enough bytes"] inp
                        else Partial (go (n'-l) s)
        in go n0 s0

-- | Skip a byte.
--
skipWord8 :: Parser ()
{-# INLINE skipWord8 #-}
skipWord8 =
    Parser $ \ kf k s inp ->
        if V.null inp
        then Partial (\ inp' ->
            if V.null inp'
            then kf ["Z.Data.Parser.Base.skipWord8: not enough bytes"] inp'
            else k s () (V.unsafeTail inp'))
        else k s () (V.unsafeTail inp)

-- | Skip past input for as long as the predicate returns 'True'.
--
skipWhile :: (Word8 -> Bool) -> Parser ()
{-# INLINE skipWhile #-}
skipWhile p =
    Parser (\ _ k s inp ->
        let !rest = V.dropWhile p inp
        in if V.null rest
            then Partial (skipWhilePartial k s)
            else k s () rest)
  where
    skipWhilePartial :: forall r. (State# ParserState -> () -> ParseStep ParseError r)
                     -> State# ParserState -> ParseStep ParseError r
    skipWhilePartial k s0 =
        let go s = \ inp ->
                if V.null inp
                then k s () inp
                else
                    let !rest = V.dropWhile p inp
                    in if V.null rest then Partial (go s) else k s () rest
        in go s0

-- | Skip over white space using 'isSpace'.
--
skipSpaces :: Parser ()
{-# INLINE skipSpaces #-}
skipSpaces = skipWhile isSpace

-- | Take N bytes.
take :: Int -> Parser V.Bytes
{-# INLINE take #-}
take n = assert (n > 0) $ readN n "Z.Data.Parser.Base.take: not enough bytes" (V.unsafeTake n)

-- | Consume input as long as the predicate returns 'False' or reach the end of input,
-- and return the consumed input.
--
takeTill :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeTill #-}
takeTill p = Parser (\ _ k s inp ->
    let (!want, !rest) = V.break p inp
    in if V.null rest
        then Partial (takeTillPartial k s want)
        else k s want rest)
  where
    takeTillPartial :: forall r. (State# ParserState -> V.PrimVector Word8 -> ParseStep ParseError r)
                    -> State# ParserState -> V.PrimVector Word8 -> ParseStep ParseError r
    takeTillPartial k s0 want =
        let go acc s = \ inp ->
                if V.null inp
                then let !r = V.concatR acc in k s r inp
                else
                    let (!want', !rest) = V.break p inp
                        acc' = want' : acc
                    in if V.null rest
                        then Partial (go acc' s)
                        else let !r = V.concatR acc' in k s r rest
        in go [want] s0

-- | Consume input as long as the predicate returns 'True' or reach the end of input,
-- and return the consumed input.
--
takeWhile :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeWhile #-}
takeWhile p = Parser (\ _ k s inp ->
    let (!want, !rest) = V.span p inp
    in if V.null rest
        then Partial (takeWhilePartial k s want)
        else k s want rest)
  where
    takeWhilePartial :: forall r. (State# ParserState -> V.PrimVector Word8 -> ParseStep ParseError r)
                     -> State# ParserState -> V.PrimVector Word8 -> ParseStep ParseError r
    takeWhilePartial k s0 want =
        let go acc s = \ inp ->
                if V.null inp
                then let !r = V.concatR acc in k s r inp
                else
                    let (!want', !rest) = V.span p inp
                        acc' = want' : acc
                    in if V.null rest
                        then Partial (go acc' s)
                        else let !r = V.concatR acc' in k s r rest
        in go [want] s0

-- | Similar to 'takeWhile', but requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or reach the end of input
--
takeWhile1 :: (Word8 -> Bool) -> Parser V.Bytes
{-# INLINE takeWhile1 #-}
takeWhile1 p = do
    bs <- takeWhile p
    if V.null bs
    then Parser (\ kf _ _ inp ->
            kf ["Z.Data.Parser.Base.takeWhile1: no satisfied byte at " <> T.toText (V.take 10 inp) ]
               inp)
    else return bs

-- | Take all the remaining input chunks and return as 'V.Bytes'.
takeRemaining :: Parser V.Bytes
{-# INLINE takeRemaining #-}
takeRemaining = Parser (\ _ k s inp -> Partial (takeRemainingPartial k s inp))
  where
    takeRemainingPartial :: forall r. (State# ParserState -> V.PrimVector Word8 -> ParseStep ParseError r)
                         -> State# ParserState -> V.PrimVector Word8 -> ParseStep ParseError r
    takeRemainingPartial k s0 want =
        let go acc s = \ inp ->
                if V.null inp
                then let !r = V.concatR acc in k s r inp
                else Partial (go (inp:acc) s)
        in go [want] s0

-- | Take N bytes and validate as UTF8, failed if not UTF8 encoded.
takeUTF8 :: Int -> Parser T.Text
{-# INLINE takeUTF8 #-}
takeUTF8 n = do
    bs <- take n
    case T.validateMaybe bs of Just t -> pure t
                               _ -> fail' $ "Z.Data.Parser.Base.takeUTF8: illegal UTF8 bytes: " <> T.toText bs

-- | Similar to 'take', but requires the predicate to succeed on next N bytes
-- of input, and take N bytes(no matter if N+1 byte satisfy predicate or not).
--
takeN :: (Word8 -> Bool) -> Int -> Parser V.Bytes
{-# INLINE takeN #-}
takeN p n = do
    bs <- take n
    if go bs 0
    then return bs
    else Parser (\ kf _ _ inp ->
        kf [ "Z.Data.Parser.Base.takeN: byte does not satisfy at " <> T.toText (bs <> V.take 10 inp) ]
            inp)

  where
    go bs@(V.PrimVector _ _ l) !i
        | i < l = p (V.unsafeIndex bs i) && go bs (i+1)
        | otherwise = True

-- | @bytes s@ parses a sequence of bytes that identically match @s@.
--
bytes :: V.Bytes -> Parser ()
{-# INLINE bytes #-}
bytes bs = do
    let n = V.length bs
    ensureN n "Z.Data.Parser.Base.bytes: not enough bytes"
    Parser (\ kf k s inp ->
        if bs == V.unsafeTake n inp
        then k s () $! V.unsafeDrop n inp
        else kf [ T.concat [
             "Z.Data.Parser.Base.bytes: mismatch bytes, expected "
            , T.toText bs
            , ", meet "
            , T.toText (V.take n inp)
            ] ] inp)


-- | Same as 'bytes' but ignoring ASCII case.
bytesCI :: V.Bytes -> Parser ()
{-# INLINE bytesCI #-}
bytesCI bs = do
    let n = V.length bs
    -- casefold an ASCII string should not change it's length
    ensureN n "Z.Data.Parser.Base.bytesCI: not enough bytes"
    Parser (\ kf k s inp ->
        if bs' == CI.foldCase (V.unsafeTake n inp)
        then k s () $! V.unsafeDrop n inp
        else kf [ T.concat [
             "Z.Data.Parser.Base.bytesCI: mismatch bytes, expected "
            , T.toText bs
            , "(case insensitive), meet "
            , T.toText (V.unsafeTake n inp)
            ] ] inp)

  where
    bs' = CI.foldCase bs

-- | @text s@ parses a sequence of UTF8 bytes that identically match @s@.
--
text :: T.Text -> Parser ()
{-# INLINE text #-}
text (T.Text bs) = bytes bs
