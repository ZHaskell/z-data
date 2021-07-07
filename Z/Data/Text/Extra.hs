{-|
Module      : Z.Data.Text.Extra
Description : Fast text slice manipulation
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Various combinators works on 'Text's.

-}

module Z.Data.Text.Extra (
  -- * Slice manipulation
    cons, snoc
  , uncons, unsnoc
  , headMaybe, tailMayEmpty, lastMaybe, initMayEmpty
  , head, tail, last, init
  , inits, tails
  , take, drop, takeR, dropR
  , slice
  , splitAt
  , takeWhile, takeWhileR, dropWhile, dropWhileR, dropAround
  , break, span
  , breakR, spanR, breakOn, breakOnAll
  , group, groupBy
  , stripPrefix, stripSuffix
  , split, splitWith, splitOn
  , isPrefixOf, isSuffixOf, isInfixOf
  , commonPrefix
  , words, lines, unwords, unlines
  , padLeft, padRight
  -- * Transform
  , reverse
  , intersperse
  , intercalate
  , intercalateElem
  , transpose
  ) where

import Data.Primitive.PrimArray
import qualified Z.Data.Vector.Base as V
import qualified Z.Data.Vector.Extra as V
import qualified Z.Data.Vector.Search as V
import Data.Coerce
import qualified Data.List as List
import Z.Data.Text.Base
import Z.Data.Text.UTF8Codec
import Z.Data.Text.Search
import           Control.Monad.ST
import           Data.Char
import           Data.Word
import           Prelude                       hiding (concat, concatMap,
                                                elem, notElem, null, length, map,
                                                foldl, foldl1, foldr, foldr1,
                                                maximum, minimum, product, sum,
                                                all, any, replicate, traverse,
                                                head, tail, init, last,
                                                take, drop, splitAt,
                                                takeWhile, dropWhile,
                                                break, span, reverse,
                                                words, lines, unwords, unlines)


--------------------------------------------------------------------------------
-- Slice manipulation

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: Char -> Text -> Text
{-# INLINE cons #-}
cons c (Text (V.PrimVector ba s l)) = Text (V.createN (4 + l) (\ mba -> do
    i <- encodeChar mba 0 c
    copyPrimArray mba i ba s l
    return $! i + l))

-- | /O(n)/ Append a char to the end of a text.
snoc :: Text -> Char -> Text
{-# INLINE snoc #-}
snoc (Text (V.PrimVector ba s l)) c = Text (V.createN (4 + l) (\ mba -> do
    copyPrimArray mba 0 ba s l
    encodeChar mba l c))

-- | /O(1)/ Extract the head and tail of a text, return 'Nothing'
-- if it is empty.
uncons :: Text -> Maybe (Char, Text)
{-# INLINE uncons #-}
uncons (Text (V.PrimVector ba s l))
    | l == 0  = Nothing
    | otherwise =
        let (# c, i #) = decodeChar ba s
        in Just (c, Text (V.PrimVector ba (s+i) (l-i)))

-- | /O(1)/ Extract the init and last of a text, return 'Nothing'
-- if text is empty.
unsnoc :: Text -> Maybe (Text, Char)
{-# INLINE unsnoc #-}
unsnoc (Text (V.PrimVector ba s l))
    | l == 0  = Nothing
    | otherwise =
        let (# c, i #) = decodeCharReverse ba (s + l - 1)
        in Just (Text (V.PrimVector ba s (l-i)), c)

-- | /O(1)/ Extract the first char of a text.
--
-- Throw 'EmptyText' if text is empty.
head :: Text -> Char
{-# INLINE head #-}
head t = case uncons t of { Just (c, _) -> c; _ ->  errorEmptyText }

-- | /O(1)/ Extract the chars after the head of a text.
--
-- Throw 'EmptyText' if text is empty.
tail :: Text -> Text
{-# INLINE tail #-}
tail t = case uncons t of { Nothing -> errorEmptyText; Just (_, t') -> t' }

-- | /O(1)/ Extract the last char of a text.
--
-- Throw 'EmptyText' if text is empty.
last :: Text ->  Char
{-# INLINE last #-}
last t = case unsnoc t of { Just (_, c) -> c; _ -> errorEmptyText }

-- | /O(1)/ Extract the chars before of the last one.
--
-- Throw 'EmptyText' if text is empty.
init :: Text -> Text
{-# INLINE init #-}
init t = case unsnoc t of { Just (t', _) -> t'; _ -> errorEmptyText }

-- | /O(1)/ Extract the first char of a text.
headMaybe :: Text -> Maybe Char
{-# INLINE headMaybe #-}
headMaybe t = case uncons t of { Just (c, _) -> Just c; _ -> Nothing }

-- | /O(1)/ Extract the chars after the head of a text.
--
-- NOTE: 'tailMayEmpty' return empty text in the case of an empty text.
tailMayEmpty :: Text -> Text
{-# INLINE tailMayEmpty #-}
tailMayEmpty t = case uncons t of { Nothing -> empty; Just (_, t') -> t' }

-- | /O(1)/ Extract the last char of a text.
lastMaybe :: Text -> Maybe Char
{-# INLINE lastMaybe #-}
lastMaybe t = case unsnoc t of { Just (_, c) -> Just c; _ -> Nothing }

-- | /O(1)/ Extract the chars before of the last one.
--
-- NOTE: 'initMayEmpty' return empty text in the case of an empty text.
initMayEmpty :: Text -> Text
{-# INLINE initMayEmpty #-}
initMayEmpty t = case unsnoc t of { Just (t', _) -> t'; _ -> empty }

-- | /O(n)/ Return all initial segments of the given text, empty first.
inits :: Text -> [Text]
{-# INLINE inits #-}
inits t0 = go t0 [t0]
  where go t acc = case unsnoc t of Just (t', _) -> go t' (t':acc)
                                    Nothing      -> acc

-- | /O(n)/ Return all final segments of the given text, whole text first.
tails :: Text -> [Text]
{-# INLINE tails #-}
tails t = t : case uncons t of Just (_, t') -> tails t'
                               Nothing      -> []

-- | /O(1)/ 'take' @n@, applied to a text @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> Text -> Text
{-# INLINE take #-}
take n t@(Text (V.PrimVector ba s _))
    | n <= 0 = empty
    | otherwise = case charByteIndex t n of i -> Text (V.PrimVector ba s (i-s))

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- char, or @[]@ if @n > 'length' xs@.
drop :: Int -> Text -> Text
{-# INLINE drop #-}
drop n t@(Text (V.PrimVector ba s l))
    | n <= 0 = t
    | otherwise = case charByteIndex t n of i -> Text (V.PrimVector ba i (l+s-i))

-- | /O(1)/ 'takeR' @n@, applied to a text @xs@, returns the suffix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
takeR :: Int -> Text -> Text
{-# INLINE takeR #-}
takeR n t@(Text (V.PrimVector ba s l))
    | n <= 0 = empty
    | otherwise = case charByteIndexR t n of i -> Text (V.PrimVector ba (i+1) (s+l-1-i))

-- | /O(1)/ 'dropR' @n xs@ returns the prefix of @xs@ before the last @n@
-- char, or @[]@ if @n > 'length' xs@.
dropR :: Int -> Text -> Text
{-# INLINE dropR #-}
dropR n t@(Text (V.PrimVector ba s _))
    | n <= 0 = t
    | otherwise = case charByteIndexR t n of i -> Text (V.PrimVector ba s (i-s+1))

-- | /O(1)/ Extract a sub-range text with give start index and length.
--
-- This function is a total function just like 'take/drop', index/length
-- exceeds range will be ingored, e.g.
--
-- @
-- slice 1 3 "hello"   == "ell"
-- slice -1 -1 "hello" == ""
-- slice -2 2 "hello"  == ""
-- slice 2 10 "hello"  == "llo"
-- @
--
-- This holds for all x y: @slice x y vs == drop x . take (x+y) vs@
slice :: Int -> Int -> Text -> Text
{-# INLINE slice #-}
slice x y t | y <= 0 = empty
            | end <= 0 = empty
            | x <= 0 = take end t
            | otherwise = take y (drop x t)
  where
    !end = x + y

-- | /O(n)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> Text -> (Text, Text)
{-# INLINE splitAt #-}
splitAt n t@(Text (V.PrimVector ba s l))
    | n <= 0 = (empty, t)
    | otherwise = case charByteIndex t n of
        i -> (Text (V.PrimVector ba s (i-s)), Text (V.PrimVector ba i (s+l-i)))


-- | /O(n)/ Applied to a predicate @p@ and a text @t@,
-- returns the longest prefix (possibly empty) of @t@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> Text -> Text
{-# INLINE takeWhile #-}
takeWhile f t@(Text (V.PrimVector arr s _)) =
    let !i = findBytesIndex (not . f) t in Text (V.PrimVector arr s i)

-- | /O(n)/ Applied to a predicate @p@ and a text @t@,
-- returns the longest suffix (possibly empty) of @t@ of elements that
-- satisfy @p@.
takeWhileR :: (Char -> Bool) -> Text -> Text
{-# INLINE takeWhileR #-}
takeWhileR f t@(Text (V.PrimVector arr s l)) =
    let !i = findBytesIndexR (not . f) t in Text (V.PrimVector arr (i+s) (l-i))

-- | /O(n)/ Applied to a predicate @p@ and a text @vs@,
-- returns the suffix (possibly empty) remaining after 'takeWhile' @p vs@.
dropWhile :: (Char -> Bool) -> Text -> Text
{-# INLINE dropWhile #-}
dropWhile f t@(Text (V.PrimVector arr _ l)) =
    let !i = findBytesIndex (not . f) t in Text (V.PrimVector arr i (l-i))

-- | /O(n)/ Applied to a predicate @p@ and a text @vs@,
-- returns the prefix (possibly empty) remaining before 'takeWhileR' @p vs@.
dropWhileR :: (Char -> Bool) -> Text -> Text
{-# INLINE dropWhileR #-}
dropWhileR f t@(Text (V.PrimVector arr s _)) =
    let !i = findBytesIndexR (not . f) t in Text (V.PrimVector arr s (i-s))

-- | /O(n)/ @dropAround f = dropWhile f . dropWhileR f@
dropAround :: (Char -> Bool) -> Text -> Text
{-# INLINE dropAround #-}
dropAround f = dropWhileR f . dropWhile f

-- | /O(n)/ Split the text into the longest prefix of elements that do not satisfy the predicate and the rest without copying.
break :: (Char -> Bool) -> Text -> (Text, Text)
{-# INLINE break #-}
break f t@(Text (V.PrimVector arr s l)) =
    let !i = findBytesIndex f t
    in (Text (V.PrimVector arr s i), Text (V.PrimVector arr i (l-i)))

-- | /O(n)/ Split the text into the longest prefix of elements that satisfy the predicate and the rest without copying.
span :: (Char -> Bool) -> Text -> (Text, Text)
{-# INLINE span #-}
span f = break (not . f)

-- | 'breakR' behaves like 'break' but from the end of the text.
--
-- @breakR p == spanR (not.p)@
breakR :: (Char -> Bool) -> Text -> (Text, Text)
{-# INLINE breakR #-}
breakR f t@(Text (V.PrimVector arr s l)) =
    let !i = findBytesIndexR f t
    in (Text (V.PrimVector arr s i), Text (V.PrimVector arr i (l-i)))

-- | 'spanR' behaves like 'span' but from the end of the text.
spanR :: (Char -> Bool) -> Text -> (Text, Text)
{-# INLINE spanR #-}
spanR f = breakR (not . f)

-- | Break a text on a subtext, returning a pair of the part of the
-- text prior to the match, and the rest of the text, e.g.
--
-- > break "wor" "hello, world" = ("hello, ", "world")
--
breakOn :: Text -> Text -> (Text, Text)
{-# INLINE breakOn #-}
breakOn (Text needle) (Text haystack) =
    case V.breakOn needle haystack of (v1, v2) -> (Text v1, Text v2)

-- | O(n+m) Find all non-overlapping instances of needle in haystack. Each element of the returned list consists of a pair:
--
--   * The entire string prior to the kth match (i.e. the prefix)
--   * The kth match, followed by the remainder of the string
--
-- Examples:
--
-- @
-- breakOnAll "::" ""
-- ==> []
-- breakOnAll "/" "a/b/c/"
-- ==> [("a", "/b/c/"), ("a/b", "/c/"), ("a/b/c", "/")]
-- @
--
-- The result list is lazy, search is performed when you force the list.
breakOnAll :: Text  -- ^ needle to search for
           -> Text  -- ^ haystack in which to search
           -> [(Text, Text)]
{-# INLINE breakOnAll #-}
breakOnAll (Text needle) (Text haystack@(V.PrimVector arr s l)) =
    List.map breaker (V.indices needle haystack False)
  where
    breaker i = (Text (V.PrimVector arr s (i-s)), Text (V.PrimVector arr i (s+l-i)))

-- | The group function takes a text and returns a list of texts such that the concatenation of the result is equal to the argument. Moreover, each sublist in the result contains only equal elements. For example,
--
-- @
-- group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
-- @
--
-- It is a special case of 'groupBy', which allows the programmer to supply their own equality test.
group :: Text -> [Text]
{-# INLINE group #-}
group = groupBy (==)

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
{-# INLINABLE groupBy #-}
groupBy f (Text (V.PrimVector arr s l))
    | l == 0    = []
    | otherwise = Text (V.PrimVector arr s (s'-s)) : groupBy f (Text (V.PrimVector arr s' (l+s-s')))
  where
    (# c0, s0 #) = decodeChar arr s
    end = s + l
    s' = go arr (s+s0)
    go arr' !i
        | i >= end = i
        | otherwise = let (# c1, s1 #) = decodeChar arr' i
                      in if f c0 c1 then go arr' (i+s1) else i

-- | /O(n)/ The 'stripPrefix' function takes two texts and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
stripPrefix :: Text -> Text -> Maybe Text
{-# INLINABLE stripPrefix #-}
stripPrefix = coerce (V.stripPrefix @V.PrimVector @Word8)


-- | O(n) The 'stripSuffix' function takes two texts and returns Just the remainder of the second iff the first is its suffix, and otherwise Nothing.
stripSuffix :: Text -> Text -> Maybe Text
{-# INLINABLE stripSuffix #-}
stripSuffix = coerce (V.stripSuffix @V.PrimVector @Word8)

-- | /O(n)/ Break a text into pieces separated by the delimiter element
-- consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
-- > split 'x'  "x"          == ["",""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- NOTE, this function behavior different with bytestring's. see
-- <https://github.com/haskell/bytestring/issues/56 #56>.
split :: Char -> Text -> [Text]
{-# INLINABLE split #-}
split x = splitWith (==x)

-- | /O(n)/ Splits a text into components delimited by
-- separators, where the predicate returns True for a separator char.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == [""]
--
splitWith :: (Char -> Bool) -> Text -> [Text]
{-# INLINABLE splitWith #-}
splitWith f (Text (V.PrimVector arr s l)) = go s s
  where
    !end = s + l
    go !p !q | q >= end  = let !v = V.PrimVector arr p (q-p) in [Text v]
             | f c       = let !v = V.PrimVector arr p (q-p) in Text v:go (q+n) (q+n)
             | otherwise = go p (q+n)
        where (# c, n #) = decodeChar arr q

-- | /O(m+n)/ Break haystack into pieces separated by needle.
--
-- Note: An empty needle will essentially split haystack element
-- by element.
--
-- Examples:
--
-- >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
-- ["a","b","d","e"]
--
-- >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
-- ["","X","X","X",""]
--
-- >>> splitOn "x"  "x"
-- ["",""]
--
-- and
--
-- > intercalate s . splitOn s         == id
-- > splitOn (singleton c)             == split (==c)
splitOn :: Text -> Text -> [Text]
{-# INLINABLE splitOn #-}
splitOn = coerce (V.splitOn @V.PrimVector @Word8)

-- | The 'isPrefix' function returns 'True' if the first argument is a prefix of the second.
isPrefixOf :: Text -> Text -> Bool
{-# INLINABLE isPrefixOf #-}
isPrefixOf = coerce (V.isPrefixOf @V.PrimVector @Word8)

-- | /O(n)/ The 'isSuffixOf' function takes two text and returns 'True'
-- if the first is a suffix of the second.
isSuffixOf :: Text -> Text -> Bool
{-# INLINABLE isSuffixOf #-}
isSuffixOf = coerce (V.isSuffixOf @V.PrimVector @Word8)

-- | Check whether one text is a subtext of another.
--
-- @needle `isInfixOf` haystack === null haystack || indices needle haystake /= []@.
isInfixOf :: Text -> Text -> Bool
{-# INLINABLE isInfixOf #-}
isInfixOf = coerce (V.isInfixOf @V.PrimVector @Word8)

-- | /O(n)/ Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match. e.g.
--
-- >>> commonPrefix "foobar" "fooquux"
-- ("foo","bar","quux")
--
-- >>> commonPrefix "veeble" "fetzer"
-- ("","veeble","fetzer")
commonPrefix :: Text -> Text -> (Text, Text, Text)
{-# INLINABLE commonPrefix #-}
commonPrefix = coerce (V.commonPrefix @V.PrimVector @Word8)

-- | /O(n)/ Breaks a 'Bytes' up into a list of words, delimited by unicode space.
words ::  Text -> [Text]
{-# INLINABLE words #-}
words (Text (V.PrimVector arr s l)) = go s s
  where
    !end = s + l
    go !s' !i | i >= end =
                    if s' == end
                    then []
                    else let !v = V.PrimVector arr s' (end-s') in [Text v]
              | otherwise =
                    let (# c, n #) = decodeChar arr i
                    in if isSpace c
                        then if s' == i
                            then go (i+n) (i+n)
                            else let !v = V.PrimVector arr s' (i-s') in Text v : go (i+n) (i+n)
                        else go s' (i+n)

-- | /O(n)/ Breaks a text up into a list of lines, delimited by ascii @\n@.
lines :: Text -> [Text]
{-# INLINABLE lines #-}
lines = coerce V.lines

-- | /O(n)/ Joins words with ascii space.
unwords :: [Text] -> Text
{-# INLINABLE unwords #-}
unwords = coerce V.unwords

-- | /O(n)/ Joins lines with ascii @\n@.
--
-- NOTE: This functions is different from 'Prelude.unlines', it DOES NOT add a trailing @\n@.
unlines :: [Text] -> Text
{-# INLINABLE unlines #-}
unlines = coerce V.unlines

-- | Add padding to the left so that the whole text's length is at least n.
padLeft :: Int -> Char -> Text -> Text
{-# INLINABLE padLeft #-}
padLeft n c t@(Text (V.PrimVector arr s l))
    | n <= tsiz = t
    | otherwise =
        let psiz = (n-tsiz)*csiz
            siz = psiz + l
        in Text (V.create siz (\ marr -> do
            _ <- encodeChar marr 0 c
            go marr csiz psiz
            copyPrimArray marr (siz-l) arr s l))
  where
    tsiz = length t
    csiz = encodeCharLength c
    go :: forall s. MutablePrimArray s Word8 -> Int -> Int -> ST s ()
    go marr s' psiz
        | s' >= psiz = return ()
        | otherwise = copyChar' csiz marr s' marr (s'-csiz) >> go marr (s'+csiz) psiz

-- | Add padding to the right so that the whole text's length is at least n.
padRight :: Int -> Char -> Text -> Text
{-# INLINABLE padRight #-}
padRight n c t@(Text (V.PrimVector arr s l))
    | n <= tsiz = t
    | otherwise =
        let psiz = (n-tsiz)*csiz
            siz = psiz + l
        in Text (V.create siz (\ marr -> do
            copyPrimArray marr 0 arr s l
            _ <- encodeChar marr l c
            go marr (l+csiz) siz))
  where
    tsiz = length t
    csiz = encodeCharLength c
    go :: forall s. MutablePrimArray s Word8 -> Int -> Int -> ST s ()
    go marr s' siz
        | s' >= siz = return ()
        | otherwise = copyChar' csiz marr s' marr (s'-csiz) >> go marr (s'+csiz) siz


--------------------------------------------------------------------------------
-- Transform

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text'. Performs replacement on invalid scalar values.
--
intersperse :: Char -> Text -> Text
{-# INLINABLE intersperse #-}
intersperse c = \ t@(Text (V.PrimVector ba s l)) ->
    let tlen = length t
    in if length t < 2
    then t
    else (runST (do
            mbaC <- newPrimArray 4 -- encoded char buf
            clen <- encodeChar mbaC 0 c
            shrinkMutablePrimArray mbaC clen
            baC <- unsafeFreezePrimArray mbaC
            let e = decodeCharLenReverse ba (s+l-1)
            return . Text $ V.create (l + (tlen-1) * clen) (go baC ba s 0 (s+l-e))
        ))
  where
    go :: PrimArray Word8  -- the encode char buf
       -> PrimArray Word8  -- the original text
       -> Int              -- decoding index of original text
       -> Int              -- writing index of new buf
       -> Int              -- the end of decoding index
       -> MutablePrimArray s Word8 -- the new buf
       -> ST s ()
    go !baC !ba !i !j !end !mba
        | i >= end = do
            let l = decodeCharLen ba i
            copyChar l mba j ba i
        | otherwise = do
            let l = decodeCharLen ba i
            copyChar l mba j ba i
            let i' = i + l
                j' = j + l
            let clen = sizeofPrimArray baC
            copyChar clen mba j' baC 0
            go baC ba i' (j'+clen) end mba

-- | /O(n)/ Reverse the characters of a string.
reverse :: Text -> Text
{-# INLINABLE reverse #-}
reverse = \ (Text (V.PrimVector ba s l)) -> Text $ V.create l (go ba s l (s+l))
  where
    go :: PrimArray Word8 -> Int -> Int -> Int -> MutablePrimArray s Word8 -> ST s ()
    go !ba !i !j !end !mba
        | i >= end = return ()
        | otherwise = do
            let l = decodeCharLen ba i
                j' = j - l
            copyChar l mba j' ba i
            go ba (i+l) j' end mba

-- | /O(n)/ The 'intercalate' function takes a 'Text' and a list of
-- 'Text's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Text -> [Text] -> Text
{-# INLINABLE intercalate #-}
intercalate s = concat . List.intersperse s

intercalateElem :: Char -> [Text] -> Text
{-# INLINABLE intercalateElem #-}
intercalateElem c = concat . List.intersperse (singleton c)

-- | The 'transpose' function transposes the rows and columns of its
-- text argument.
--
transpose :: [Text] -> [Text]
{-# INLINABLE transpose #-}
transpose ts = List.map pack . List.transpose . List.map unpack $ ts

