{-|
Module      : Z.Data.Text
#ifdef CN_DOC
Description : Unicode 文字处理
#else
Description : Unicode text processing
#endif
Copyright   : (c) Dong Han, 2017-2023
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

#ifdef CN_DOC
本模块使用和 @text >= 2.0@ 相同的 'Text' 类型，并提供了额外的 API（和 "Z.Data.Vector"更一致）。'Text' 类型
包装了使用 UTF-8 编码的二进制数据，用户可以通过 'validate' 来构造 'Text'。
#else
This module use the same 'Text' type from @text >= 2.0@ package, but provide extra API and more consistent with "Z.Data.Vector". 'Text' wrap bytes which will be interpreted using UTF-8 encoding. User can use 'validate' to construt a 'Text', illegal UTF-8 encoded codepoints will cause undefined behaviours.
#endif
>>> :set -XOverloadedStrings
>>> import qualified Z.Data.Text as T
>>> "你好世界" T.!: (0, 2)
"\20320\22909"
-}

module Z.Data.Text (
  -- * Text type
    Text(UTF8Slice)
  , getUTF8Bytes, unsafeFromUTF8Bytes
  -- * UTF8 Validation
  , validate, validateASCII, validateMaybe, validateASCIIMaybe
  -- * Conversion between list & vector
  , pack, packN, packR, packRN, unpack, unpackR
  , fromCharVector, toCharVector
  -- * Basic interface
  , index', (!!), index, (!?), slice, (!:), splitAt, (!/), take, drop
  , cons, snoc, uncons, unsnoc, inits, tails
  , null , length , append, concat, concatR, concatMap
  , shuffle, permutations, reverse, intersperse, intercalate, intercalateElem, transpose
  , takeWhile, takeWhileR, dropWhile, dropWhileR, dropAround
  , filter, partition, break, breakR, breakOn, span, spanR
  , split, splitWith, splitOn
  , isPrefixOf, isSuffixOf, isInfixOf, commonPrefix, stripPrefix, stripSuffix
  , group, groupBy, words, unwords, lines, unlines, padLeft, padRight
  -- *  Creation utilies
  , empty, singleton, copy, replicate, cycle
  -- * Map, Fold, Traverse
  , map, imap
  , foldl', ifoldl', foldr'
  , count, all, any
  -- * Search element and sub-slice
  , elem, notElem, findIndices, findChar, findCharR, breakOnAll
  -- * Text display width
  , displayWidth, displayWidthChar
  -- * Normalization, Case conversion, Collation
  , NormalizationResult(..), NormalizeMode(..)
  , isNormalized, isNormalizedTo, normalize, normalizeTo
  , caseFold, caseFoldWith, toLower, toLowerWith, toUpper, toUpperWith, toTitle, toTitleWith
  , isCategory, spanCategory
  , Collator(..), collate
  , module Text.Collate
  -- * Locale & Category constants
  , Locale
  , envLocale
  , pattern LocaleDefault
  , pattern LocaleLithuanian
  , pattern LocaleTurkishAndAzeriLatin
  , Category
  , pattern CategoryLetterUppercase
  , pattern CategoryLetterLowercase
  , pattern CategoryLetterTitlecase
  , pattern CategoryLetterOther
  , pattern CategoryLetter
  , pattern CategoryCaseMapped
  , pattern CategoryMarkNonSpacing
  , pattern CategoryMarkSpacing
  , pattern CategoryMarkEnclosing
  , pattern CategoryMark
  , pattern CategoryNumberDecimal
  , pattern CategoryNumberLetter
  , pattern CategoryNumberOther
  , pattern CategoryNumber
  , pattern CategoryPunctuationConnector
  , pattern CategoryPunctuationDash
  , pattern CategoryPunctuationOpen
  , pattern CategoryPunctuationClose
  , pattern CategoryPunctuationInitial
  , pattern CategoryPunctuationFinal
  , pattern CategoryPunctuationOther
  , pattern CategoryPunctuation
  , pattern CategorySymbolMath
  , pattern CategorySymbolCurrency
  , pattern CategorySymbolModifier
  , pattern CategorySymbolOther
  , pattern CategorySymbol
  , pattern CategorySeparatorSpace
  , pattern CategorySeparatorLine
  , pattern CategorySeparatorParagraph
  , pattern CategorySeparator
  , pattern CategoryControl
  , pattern CategoryFormat
  , pattern CategorySurrogate
  , pattern CategoryPrivateUse
  , pattern CategoryUnassigned
  , pattern CategoryCompatibility
  , pattern CategoryIgnoreGraphemeCluster
  , pattern CategoryIscntrl
  , pattern CategoryIsprint
  , pattern CategoryIsspace
  , pattern CategoryIsblank
  , pattern CategoryIsgraph
  , pattern CategoryIspunct
  , pattern CategoryIsalnum
  , pattern CategoryIsalpha
  , pattern CategoryIsupper
  , pattern CategoryIslower
  , pattern CategoryIsdigit
  , pattern CategoryIsxdigit
  -- * Misc
  , TextException(..), errorEmptyText
 ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.Function             (on)
import           Data.Maybe                (isJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Array           as T hiding (empty)
import qualified Data.Text.Internal        as T hiding (empty)
import           Data.Bits
import           Data.Char                 hiding (toLower, toTitle, toUpper)
import           Data.Foldable             (foldlM)
import           Data.Int
import qualified Data.List                 as List
import           Data.Primitive.PrimArray  hiding (copyPtrToMutablePrimArray)
import           Data.Typeable
import           Data.Word
import           GHC.Exts
import           GHC.Stack
import           System.IO.Unsafe          (unsafeDupablePerformIO)
import           System.Random.Stateful    (StatefulGen)
import           Test.QuickCheck.Instances.Text ()
import           Z.Data.Array
import           Z.Data.ASCII              (c2w)
import           Z.Data.Utils.FFI          (c_strlen, z_utf8_validate, z_ascii_validate,
                                            z_utf8_validate_addr, z_utf8_isnormalized,
                                            z_utf8_normalize_length, z_utf8_normalize,
                                            z_utf8_casefold_length, z_utf8_casefold,
                                            z_utf8_tolower_length, z_utf8_tolower,
                                            z_utf8_toupper_length, z_utf8_toupper,
                                            z_utf8_totitle_length, z_utf8_totitle,
                                            z_utf8_iscategory, z_wcwidth)
import           Z.Data.Utils.UTF8Codec
import           Z.Data.Utils.UTF8Rewind
import qualified Z.Data.Vector             as V
import           Z.Data.Vector             (Bytes, PrimVector (..), IPair(..))
import           Prelude                   hiding (all, any, concat, concatMap,
                                            elem, foldl, foldl1, foldr, foldr1,
                                            length, map, maximum, minimum, (!!),
                                            notElem, null, product, replicate, cycle,
                                            sum, traverse, splitAt, take, drop,
                                            takeWhile, dropWhile,
                                            break, span, reverse, filter,
                                            words, lines, unwords, unlines)
import           Text.Collate              hiding (collate)

-- $setup
-- >>> import Test.QuickCheck.Modifiers
-- >>> import Test.QuickCheck.Function
-- >>> :set -XOverloadedStrings

primArrayToByteArray :: PrimArray Word8 -> T.Array
{-# INLINE primArrayToByteArray #-}
primArrayToByteArray (PrimArray arr) = T.ByteArray arr

byteArrayToPrimArray :: T.Array -> PrimArray Word8
{-# INLINE byteArrayToPrimArray #-}
byteArrayToPrimArray (T.ByteArray arr) = PrimArray arr

pattern UTF8Slice :: PrimArray Word8 -> Int -> Int -> Text
pattern UTF8Slice pa s l <- (T.Text (byteArrayToPrimArray -> pa) s l) where
        UTF8Slice (primArrayToByteArray -> ba) s l = T.Text ba s l
{-# COMPLETE UTF8Slice #-}

getUTF8Bytes :: Text -> Bytes
{-# INLINE getUTF8Bytes #-}
getUTF8Bytes (T.Text ba s l) = PrimVector (byteArrayToPrimArray ba) s l

unsafeFromUTF8Bytes :: Bytes -> Text
{-# INLINE unsafeFromUTF8Bytes #-}
unsafeFromUTF8Bytes (PrimVector arr s l) = T.Text (primArrayToByteArray arr) s l

-- | /O(n)/ Get the nth codepoint from 'Text', throw 'IndexOutOfTextRange'
-- when out of bound.
index' :: HasCallStack => Text -> Int -> Char
{-# INLINE index' #-}
index' t n = case t `index` n of Nothing -> throw (IndexOutOfTextRange n callStack)
                                 Just x  -> x
{-|
#ifdef CN_DOC
中缀版本 'index''
#else
Infix 'index''
#endif
>>> "你好世界" :: Text
"\20320\22909\19990\30028"
>>> "你好世界" !! 1
'\22909'
>>> "你好世界" !! (-1)
'\30028'
>>> "你好世界" !! 10
*** Exception: IndexOutOfTextRange...
-}
(!!) :: HasCallStack => Text -> Int -> Char
(!!) = index'

-- | /O(n)/ Get the nth codepoint from 'Text'.
index :: Text -> Int -> Maybe Char
{-# INLINE index #-}
index (UTF8Slice ba s l) n
    | n < 0 = goR (end-1) (-1)
    | otherwise = go s 0
  where
    !end = s + l
    go !i !j
        | i >= end = Nothing
        | j >= n = let !c = decodeChar_ ba i in Just c
        | otherwise = let l' = decodeCharLen ba i in go (i+l') (j+1)
    goR !i !j
        | i < s = Nothing
        | j <= n = let !c = decodeCharReverse_ ba i in Just c
        | otherwise = let l' = decodeCharLenReverse ba i in goR (i-l') (j-1)

{-|
#ifdef CN_DOC
中缀版本 'index'
#else
Infix 'index'
#endif
>>> "你好世界" :: Text
"\20320\22909\19990\30028"
>>> "你好世界" !? 1
Just '\22909'
>>> "你好世界" !? (-1)
Just '\30028'
>>> "你好世界" !? 10
Nothing
-}
(!?) :: HasCallStack => Text -> Int -> Maybe Char
(!?) = index

--------------------------------------------------------------------------------
-- Slice manipulation

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: Char -> Text -> Text
{-# INLINE cons #-}
cons c (UTF8Slice ba s l) = unsafeFromUTF8Bytes (V.createN (4 + l) (\ mba -> do
    i <- encodeChar mba 0 c
    copyPrimArray mba i ba s l
    return $! i + l))

-- | /O(n)/ Append a char to the end of a text.
snoc :: Text -> Char -> Text
{-# INLINE snoc #-}
snoc (UTF8Slice ba s l) c = unsafeFromUTF8Bytes (V.createN (4 + l) (\ mba -> do
    copyPrimArray mba 0 ba s l
    encodeChar mba l c))

-- | /O(1)/ Extract the head and tail of a text, return 'Nothing'
-- if it is empty.
uncons :: Text -> Maybe (Char, Text)
{-# INLINE uncons #-}
uncons (UTF8Slice ba s l)
    | l == 0  = Nothing
    | otherwise =
        let (# c, i #) = decodeChar ba s
        in Just (c, UTF8Slice ba (s+i) (l-i))

-- | /O(1)/ Extract the init and last of a text, return 'Nothing'
-- if text is empty.
unsnoc :: Text -> Maybe (Text, Char)
{-# INLINE unsnoc #-}
unsnoc (UTF8Slice ba s l)
    | l == 0  = Nothing
    | otherwise =
        let (# c, i #) = decodeCharReverse ba (s + l - 1)
        in Just (UTF8Slice ba s (l-i), c)


-- | /O(n)/ Find the nth codepoint's byte index (pointing to the nth char's begining byte).
--
-- The index is only meaningful to the whole byte slice, if there's less than n codepoints,
-- the index will point to next byte after the end.
charByteIndex :: Text -> Int -> Int
{-# INLINE charByteIndex #-}
charByteIndex (UTF8Slice ba s l) n
    | n < 0 = goR (end-1) (-1)
    | otherwise = go s 0
  where
    !end = s + l
    go !i !j
        | i >= end = i
        | j >= n = i
        | otherwise = let l' = decodeCharLen ba i in go (i+l') (j+1)
    goR !i !j
        | i < s = i + 1
        | j <= n = i - decodeCharLenReverse ba i + 1
        | otherwise = let l' = decodeCharLenReverse ba i in goR (i-l') (j-1)

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

{-|
#ifdef CN_DOC
/O(n)/ 提取文字从 0 到 i （不包含）的字符，支持负数下标（末尾从 @-1@ 开始计算）。
#else
/O(1)/ Take first n chars, negative index is supported(equivalent to taking from 0 to last @-n@ chars).
This function is a total function, index exceeds range will be ingored, e.g.
#endif

@
   +----- take 5 ------+                        
   0    1    2    3    4    5    6    7    8    9   10
  "h    e    l    l    o    ,    你   好   世   界  ！"
 -11   -10  -9   -8   -7   -6   -5   -4   -3   -2   -1
   +-------------- take (-2) --------------+
@
>>> take 5 "hello,你好世界！"
"hello"
>>> take (-2) "hello,你好世界！"
"hello,你好世"
-}
take :: Int -> Text -> Text
{-# INLINE take #-}
take n t@(UTF8Slice ba s _) = case charByteIndex t n of i -> UTF8Slice ba s (i-s)
-- $property
-- props> \ (Positive n) -> take n t = pack $ List.take n (unpack t)
-- props> \ (Negative n) -> take n t = pack $ List.take (List.length + n) (unpack t)
-- props> take x = slice 0 x t

{-|
#ifdef CN_DOC
/O(n)/ 丢掉文本从 0 到 i（不包含）的字符，支持负数下标（末尾从 @-1@ 开始计算）。
#else
/O(1)/ Drop first n chars, negative length is supported(equivalent to dropping from 0 to last @-n@ chars).
This function is a total function, index exceeds range will be ingored, e.g.
#endif
>>> drop 5 "hello,你好世界！"
",你好世界！"
>>> drop (-3) "hello,你好世界！"
"世界！"
>>> drop 20 "hello,你好世界！"
""
@
                            +-------- drop 5 --------+
   0    1    2    3    4    5    6    7    8    9   10
  "h    e    l    l    o    ,    你   好   世   界  ！"
 -11   -10  -9   -8   -7   -6   -5   -4   -3   -2   -1
                                           +drop (-3)+
@
-}
drop :: Int -> Text -> Text
{-# INLINE drop #-}
drop n t@(UTF8Slice ba s l) = case charByteIndex t n of i -> UTF8Slice ba i (s+l-i)
-- $property
-- props> \ (Positive n) -> drop n t = pack $ List.drop n (unpack t)
-- props> \ (Negative n) -> drop n t = pack $ List.drop (List.length + n) (unpack t)
-- props> drop x t = slice x (length t) t

{-|
#ifdef CN_DOC
/O(1)/ 创建起始下标（包含）到结束下标（不包含）的新的文本切片，支持负数下标（含义和 'index', 'take' 一致），这是一个全函数，超出范围的下标会被忽略：
#else
/O(1)/ Extract a sub-range text with give start index(included) and end index(not included).
This function is a total function just like 'take/drop', start and end index
exceeds range will be ingored, e.g.
#endif
>>> slice 1 3 "hello"
"el"
>>> slice 1 (-1) "hello"
"ell"
>>> slice (-4) (-2) "hello"
"el"
>>> slice 2 10 "hello"
"llo"
-}
slice :: Int          -- ^ slice start index
      -> Int          -- ^ slice end index
      -> Text -> Text
{-# INLINE slice #-}
slice x y (UTF8Slice ba s l) =
    if (x >= 0)
    then if (y >= 0)
        then if (x < y)
            then let i = go x s 0;
                     j = go y i x
                 in UTF8Slice ba i (j-i)
            else empty
        else let i = go x s 0
                 j = goR y (end-1) (-1)
             in UTF8Slice ba i (j-i)
    else if (y >= 0)
        then let i = goR x (end-1) (-1)
                 j = go y s 0
             in if i < j then UTF8Slice ba i (j-i) else empty
        else if (x < y)
            then let j = goR y (end-1) (-1)
                     i = goR x (j-1) (y-1)
                 in UTF8Slice ba i (j-i)
            else empty
  where
    !end = s + l
    go !m !i !j
        | i >= end = i
        | j >= m = i
        | otherwise = let l' = decodeCharLen ba i in go m (i+l') (j+1)
    goR !m !i !j
        | i < s = i + 1
        | j <= m = i - decodeCharLenReverse ba i + 1
        | otherwise = let l' = decodeCharLenReverse ba i in goR m (i-l') (j-1)
-- $property:  slice x y === drop x . take y
-- prop> \ (Positive x) (Positive y) -> slice x y t == (drop x . take y) t
-- prop> \ (Negative x) (Negative y) -> slice x y t == (take y . drop x) t


{-|
#ifdef CN_DOC
中缀版本'slice'。
#else
Infix version of 'slice'.
#endif
>>> "hello" !: (1, 3)
"el"
>>> "hello" !: (1, -1)
"ell"
>>> "hello" !: (-5, 2)
"he"
>>> "hello" !: (-2, 2)
""
>>> "hello" !: (2, 10)
"llo"
-}
(!:) :: Text 
     -> (Int, Int)     -- ^ start and end index 
     -> Text
{-# INLINE (!:) #-}
t !: (s, e) = slice s e t

{-|
#ifdef CN_DOC
/O(m)/ 在指定下标处分割文本，@'splitAt' n x == ('take' n xs, 'drop' n xs)@
#else
/O(m)/ Split text at given index. @'splitAt' n x == ('take' n xs, 'drop' n xs)@
#endif
>>> splitAt 3 "hello"
("hel","lo")
>>> splitAt 0 "hello"
("","hello")
>>> splitAt (-1) "hello"
("hell","o")
>>> splitAt 10 "hello"
("hello","")
-}
splitAt :: Int -> Text -> (Text, Text)
{-# INLINE splitAt #-}
splitAt n t@(UTF8Slice ba s l) =
    case charByteIndex t n of i -> (UTF8Slice ba s (i-s), UTF8Slice ba i (s+l-i))
-- $property: splitAt n xs == (take n xs, drop n xs)
-- prop> splitAt n xs == (take n xs, drop n xs)

{-|
#ifdef CN_DOC
中缀版本'splitAt'。
#else
Infix version of 'splitAt'.
#endif
>>> "hello" !/ 3
("hel","lo")
>>> "hello" !/ 0
("","hello")
>>> "hello" !/ (-1)
("hell","o")
>>> "hello" !/ 10
("hello","")
-}
(!/) :: Text -> Int -> (Text, Text)
{-# INLINE (!/) #-}
v !/ x = splitAt x v

--------------------------------------------------------------------------------

-- | find all char index matching the predicate.
findIndices :: (Char -> Bool) -> Text -> [Int]
{-# INLINE findIndices #-}
findIndices f (UTF8Slice arr s l) = go 0 s
  where
    !end = s + l
    go !i !p | p >= end  = []
             | f x       = i : go (i+1) (p+off)
             | otherwise = go (i+1) (p+off)
        where (# x, off #) = decodeChar arr p

-- | /O(n)/ find the first char matching the predicate in a text
-- from left to right, if there isn't one, return the text length.
findChar :: (Char -> Bool)
         -> Text
         -> Maybe (IPair Char)  -- ^ (char index, matching char)
{-# INLINE findChar #-}
findChar f (UTF8Slice arr s l) = go 0 s
  where
    !end = s + l
    go !i !j | j >= end  = Nothing
             | otherwise =
                let (# x, off #) = decodeChar arr j
                in if f x
                    then Just (IPair i x)
                    else go (i+1) (j+off)

-- | /O(n)/ find the first char matching the predicate in a text
-- from right to left.
findCharR :: (Char -> Bool)
          -> Text
          -> Maybe (IPair Char)  -- ^ (char index(counting backwards), matching char)
{-# INLINE findCharR #-}
findCharR f (UTF8Slice arr s l) = go 0 (s+l-1)
  where
    go !i !j | j < s     = Nothing
             | otherwise =
                let (# x, off #) = decodeCharReverse arr j
                in if f x
                    then Just (IPair i x)
                    else go (i+1) (j-off)

--------------------------------------------------------------------------------

-- | /O(n)/ find the char's byte slice index.
findBytesIndex :: (Char -> Bool) -> Text -> Int
{-# INLINABLE findBytesIndex #-}
findBytesIndex f (UTF8Slice arr s l) = go s
  where
    !end = s + l
    go !j | j >= end  = j-s
          | otherwise =
              let (# x, off #) = decodeChar arr j
              in if f x
                  then j-s
                  else go (j+off)

-- | /O(n)/ find the char's byte slice index in reverse order(pointing to the right char's first byte).
findBytesIndexR ::  (Char -> Bool) -> Text -> Int
{-# INLINABLE findBytesIndexR #-}
findBytesIndexR f (UTF8Slice arr s l) = go (s+l-1)
  where
    go !j | j < s     = j-s+1
          | otherwise =
              let (# x, off #) = decodeCharReverse arr j
              in if f x
                  then j-s+1
                  else go (j-off)

-- | /O(n)/ 'filter', applied to a predicate and a text,
-- returns a text containing those chars that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Text -> Text
{-# INLINABLE filter #-}
filter f (UTF8Slice arr s l) = unsafeFromUTF8Bytes (V.createN l (go s 0))
  where
    !end = s + l
    go :: Int -> Int -> MutablePrimArray s Word8 -> ST s Int
    go !i !j marr
        | i >= end = return j
        | otherwise =
            let (# x, off #) = decodeChar arr i
            in if f x
                then do
                    copyChar off marr j arr i
                    go (i+off) (j+off) marr
                else go (i+off) j marr

-- | /O(n)/ The 'partition' function takes a predicate, a text, returns
-- a pair of text with codepoints which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p txt == (filter p txt, filter (not . p) txt)
partition :: (Char -> Bool) -> Text -> (Text, Text)
{-# INLINABLE partition #-}
partition f (UTF8Slice arr s l)
    | l == 0    = (empty, empty)
    | otherwise = runST $ do
        mba0 <- newMutableArray l
        mba1 <- newMutableArray l
        let !end = s + l
            go !i !j !p =
                if (p >= end)
                then do
                    shrinkMutableArray mba0 i
                    shrinkMutableArray mba1 j
                    ba0 <- unsafeFreezeMutableArray mba0
                    ba1 <- unsafeFreezeMutableArray mba1
                    return (UTF8Slice ba0 0 i, UTF8Slice ba1 0 j)
                else case decodeChar arr p of
                    (# x, off #) ->
                        if f x
                        then copyChar off mba0 i arr p >> go (i+off) j (p+off) 
                        else copyChar off mba1 j arr p >> go i (j+off) (p+off) 
        go 0 0 s

--------------------------------------------------------------------------------
-- Searching by equality

-- | /O(n)/ 'elem' test if given char is in given text.
elem :: Char -> Text -> Bool
{-# INLINABLE elem #-}
elem x t = isJust (findChar (x==) t)

-- | /O(n)/ @not . elem@
notElem ::  Char -> Text -> Bool
{-# INLINABLE notElem #-}
notElem x = not . elem x


-- | /O(n)/ Applied to a predicate @p@ and a text @t@,
-- returns the longest prefix (possibly empty) of @t@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> Text -> Text
{-# INLINE takeWhile #-}
takeWhile f t@(UTF8Slice arr s _) =
    let !i = findBytesIndex (not . f) t in UTF8Slice arr s i

-- | /O(n)/ Applied to a predicate @p@ and a text @t@,
-- returns the longest suffix (possibly empty) of @t@ of elements that
-- satisfy @p@.
takeWhileR :: (Char -> Bool) -> Text -> Text
{-# INLINE takeWhileR #-}
takeWhileR f t@(UTF8Slice arr s l) =
    let !i = findBytesIndexR (not . f) t in UTF8Slice arr (i+s) (l-i)

-- | /O(n)/ Applied to a predicate @p@ and a text @vs@,
-- returns the suffix (possibly empty) remaining after 'takeWhile' @p vs@.
dropWhile :: (Char -> Bool) -> Text -> Text
{-# INLINE dropWhile #-}
dropWhile f t@(UTF8Slice arr _ l) =
    let !i = findBytesIndex (not . f) t in UTF8Slice arr i (l-i)

-- | /O(n)/ Applied to a predicate @p@ and a text @vs@,
-- returns the prefix (possibly empty) remaining before 'takeWhileR' @p vs@.
dropWhileR :: (Char -> Bool) -> Text -> Text
{-# INLINE dropWhileR #-}
dropWhileR f t@(UTF8Slice arr s _) =
    let !i = findBytesIndexR (not . f) t in UTF8Slice arr s (i-s)

-- | /O(n)/ @dropAround f = dropWhile f . dropWhileR f@
dropAround :: (Char -> Bool) -> Text -> Text
{-# INLINE dropAround #-}
dropAround f = dropWhileR f . dropWhile f

-- | /O(n)/ Split the text into the longest prefix of elements that do not satisfy the predicate and the rest without copying.
break :: (Char -> Bool) -> Text -> (Text, Text)
{-# INLINE break #-}
break f t@(UTF8Slice arr s l) =
    let !i = findBytesIndex f t
    in (UTF8Slice arr s i, UTF8Slice arr i (l-i))

-- | /O(n)/ Split the text into the longest prefix of elements that satisfy the predicate and the rest without copying.
span :: (Char -> Bool) -> Text -> (Text, Text)
{-# INLINE span #-}
span f = break (not . f)

-- | 'breakR' behaves like 'break' but from the end of the text.
--
-- @breakR p == spanR (not.p)@
breakR :: (Char -> Bool) -> Text -> (Text, Text)
{-# INLINE breakR #-}
breakR f t@(UTF8Slice arr s l) =
    let !i = findBytesIndexR f t
    in (UTF8Slice arr s i, UTF8Slice arr i (l-i))

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
breakOn (getUTF8Bytes -> needle) (getUTF8Bytes -> haystack) =
    case V.breakOn needle haystack of (v1, v2) -> (unsafeFromUTF8Bytes v1, unsafeFromUTF8Bytes v2)

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
breakOnAll (getUTF8Bytes -> needle) (getUTF8Bytes -> haystack@(V.PrimVector arr s l)) =
    List.map breaker (V.indices False needle haystack)
  where
    breaker i = (UTF8Slice arr s (i-s), UTF8Slice arr i (s+l-i))

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
groupBy f (UTF8Slice arr s l)
    | l == 0    = []
    | otherwise = UTF8Slice arr s (s'-s) : groupBy f (UTF8Slice arr s' (l+s-s'))
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
stripPrefix :: Text -> Text -> Text
{-# INLINE stripPrefix #-}
stripPrefix (getUTF8Bytes -> prefix) (getUTF8Bytes -> haystack) = 
    unsafeFromUTF8Bytes (V.stripPrefix prefix haystack)


-- | O(n) The 'stripSuffix' function takes two texts and returns Just the remainder of the second iff the first is its suffix, and otherwise Nothing.
stripSuffix :: Text -> Text -> Text
{-# INLINE stripSuffix #-}
stripSuffix (getUTF8Bytes -> prefix) (getUTF8Bytes -> haystack) = 
    unsafeFromUTF8Bytes (V.stripSuffix prefix haystack)

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
splitWith f (UTF8Slice arr s l) = go s s
  where
    !end = s + l
    go !p !q | q >= end  = let !t = UTF8Slice arr p (q-p) in [t]
             | f c       = let !t = UTF8Slice arr p (q-p) in t:go (q+n) (q+n)
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
splitOn (getUTF8Bytes -> needle) (getUTF8Bytes -> haystack) = 
    unsafeFromUTF8Bytes <$> V.splitOn needle haystack

-- | The 'isPrefix' function returns 'True' if the first argument is a prefix of the second.
isPrefixOf :: Text -> Text -> Bool
{-# INLINABLE isPrefixOf #-}
isPrefixOf = V.isPrefixOf `on` getUTF8Bytes

-- | /O(n)/ The 'isSuffixOf' function takes two text and returns 'True'
-- if the first is a suffix of the second.
isSuffixOf :: Text -> Text -> Bool
{-# INLINABLE isSuffixOf #-}
isSuffixOf = V.isSuffixOf `on` getUTF8Bytes

-- | Check whether one text is a subtext of another.
--
-- @needle `isInfixOf` haystack === null haystack || indices needle haystake /= []@.
isInfixOf :: Text -> Text -> Bool
{-# INLINABLE isInfixOf #-}
isInfixOf = V.isInfixOf `on` getUTF8Bytes

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
commonPrefix (getUTF8Bytes -> needle) (getUTF8Bytes -> haystack) = 
    case V.commonPrefix needle haystack of 
        (t1, t2, t3) -> (unsafeFromUTF8Bytes t1, unsafeFromUTF8Bytes t2, unsafeFromUTF8Bytes t3)

-- | /O(n)/ Breaks a 'Bytes' up into a list of words, delimited by unicode space.
words ::  Text -> [Text]
{-# INLINABLE words #-}
words = T.words

-- | /O(n)/ Breaks a text up into a list of lines, delimited by ascii @\n@.
lines :: Text -> [Text]
{-# INLINABLE lines #-}
lines (getUTF8Bytes -> v) = unsafeFromUTF8Bytes <$> V.lines v

-- | /O(n)/ Joins words with ascii space.
unwords :: [Text] -> Text
{-# INLINABLE unwords #-}
unwords = intercalateElem ' '

-- | /O(n)/ Joins lines with ascii @\n@.
--
-- NOTE: This functions is different from 'Prelude.unlines', it DOES NOT add a trailing @\n@.
unlines :: [Text] -> Text
{-# INLINABLE unlines #-}
unlines = intercalateElem '\n'

-- | Add padding to the left so that the whole text's length is at least n.
padLeft :: Int -> Char -> Text -> Text
{-# INLINABLE padLeft #-}
padLeft = T.justifyRight

-- | Add padding to the right so that the whole text's length is at least n.
padRight :: Int -> Char -> Text -> Text
{-# INLINABLE padRight #-}
padRight = T.justifyLeft 

--------------------------------------------------------------------------------
-- Transform

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text'. Performs replacement on invalid scalar values.
--
intersperse :: Char -> Text -> Text
{-# INLINABLE intersperse #-}
intersperse = T.intersperse

-- | /O(n)/ Reverse the characters of a string.
reverse :: Text -> Text
{-# INLINABLE reverse #-}
reverse = T.reverse

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


--------------------------------------------------------------------------------

-- | /O(n)/ Validate a sequence of bytes is UTF-8 encoded.
--
-- Throw 'InvalidUTF8Exception' in case of invalid codepoint.
--
validate :: HasCallStack => Bytes -> Text
{-# INLINE validate #-}
validate bs@(V.PrimVector (PrimArray ba#) (I# s#) l@(I# l#))
    | l == 0 = empty
    | z_utf8_validate ba# s# l# > 0 = unsafeFromUTF8Bytes bs
    | otherwise = throw (InvalidUTF8Exception callStack)

-- | /O(n)/ Validate a sequence of bytes is UTF-8 encoded.
--
-- Return 'Nothing' in case of invalid codepoint.
--
validateMaybe :: Bytes -> Maybe Text
{-# INLINE validateMaybe #-}
validateMaybe bs@(V.PrimVector (PrimArray ba#) (I# s#) l@(I# l#))
    | l == 0 = Just empty
    | z_utf8_validate ba# s# l# > 0 = Just (unsafeFromUTF8Bytes bs)
    | otherwise = Nothing

-- | /O(n)/ Validate a sequence of bytes is all ascii char byte(<128).
--
-- Throw 'InvalidASCIIException' in case of invalid byte, It's not always faster
-- than 'validate', use it only if you want to validate ASCII char sequences.
--
validateASCII :: HasCallStack => Bytes -> Text
{-# INLINE validateASCII #-}
validateASCII bs@(V.PrimVector (PrimArray ba#) (I# s#) l@(I# l#))
    | l == 0 = empty
    | z_ascii_validate ba# s# l# > 0 = unsafeFromUTF8Bytes bs
    | otherwise = throw (InvalidASCIIException callStack)

-- | /O(n)/ Validate a sequence of bytes is all ascii char byte(<128).
--
-- Return 'Nothing' in case of invalid byte.
--
validateASCIIMaybe :: Bytes -> Maybe Text
{-# INLINE validateASCIIMaybe #-}
validateASCIIMaybe bs@(V.PrimVector (PrimArray ba#) (I# s#) l@(I# l#))
    | l == 0 = Just empty
    | z_ascii_validate ba# s# l# > 0 = Just (unsafeFromUTF8Bytes bs)
    | otherwise = Nothing


data TextException = InvalidUTF8Exception CallStack
                   | InvalidASCIIException CallStack
                   | IndexOutOfTextRange Int CallStack
                   | EmptyText CallStack
                  deriving (Show, Typeable)
instance Exception TextException

errorEmptyText :: HasCallStack => a
{-# INLINE errorEmptyText #-}
errorEmptyText = throw (EmptyText callStack)

--------------------------------------------------------------------------------

-- | /O(n)/ Convert a string into a text
--
-- Alias for @'packN' 'defaultInitSize'@, will be rewritten to a memcpy if possible.
pack :: String -> Text
pack = packN V.defaultInitSize
{-# INLINE CONLIKE [0] pack #-}
{-# RULES "pack/packASCIIAddr" forall addr . pack (unpackCString# addr) = packASCIIAddr addr #-}
{-# RULES "pack/packUTF8Addr" forall addr . pack (unpackCStringUtf8# addr) = packUTF8Addr addr #-}

packASCIIAddr :: Addr# -> Text
{-# INLINE packASCIIAddr #-}
packASCIIAddr addr0# = go addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ c_strlen addr0#
    go addr# = runST $ do
        marr <- newPrimArray len
        copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
        arr <- unsafeFreezePrimArray marr
        return $ UTF8Slice arr 0 len

packUTF8Addr :: Addr# -> Text
{-# INLINE packUTF8Addr #-}
packUTF8Addr addr0# = validateAndCopy addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ c_strlen addr0#
    valid = unsafeDupablePerformIO $ z_utf8_validate_addr addr0# len
    validateAndCopy addr#
        | valid == 0 = packN len (unpackCStringUtf8# addr#) -- three bytes surrogate -> three bytes replacement
                                                            -- two bytes NUL -> \NUL
                                                            -- the result's length will either smaller or equal
        | otherwise  = runST $ do
            marr <- newPrimArray len
            copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
            arr <- unsafeFreezePrimArray marr
            return $ UTF8Slice arr 0 len

-- | /O(n)/ Convert a list into a text with an approximate size(in bytes, not codepoints).
--
-- If the encoded bytes length is larger than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: Int -> String -> Text
{-# INLINE packN #-}
packN n0 = \ ws0 -> runST $ do
        marr <- newMutableArray (max 4 n0)
        (IPair l marr') <- foldlM go (IPair 0 marr) ws0
        shrinkMutableArray marr' l
        arr <- unsafeFreezeMutableArray marr'
        return $! UTF8Slice arr 0 l
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: IPair (MutablePrimArray s Word8) -> Char -> ST s (IPair (MutablePrimArray s Word8))
    go (IPair i marr) !c = do
        siz <- getSizeofMutablePrimArray marr
        if i < siz - 3  -- we need at least 4 bytes for safety
        then do
            i' <- encodeChar marr i c
            return (IPair i' marr)
        else do
            let !siz' = siz `shiftL` 1
            !marr' <- resizeMutablePrimArray marr siz'
            i' <- encodeChar marr' i c
            return (IPair i' marr')

-- | /O(n)/ Alias for @'packRN' 'defaultInitSize'@.
--
packR :: String -> Text
{-# INLINE packR #-}
packR = packRN V.defaultInitSize

-- | /O(n)/ 'packN' in reverse order.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packRN :: Int -> String -> Text
{-# INLINE packRN #-}
packRN n0 = \ ws0 -> runST (do let n = max 4 n0
                               marr <- newMutableArray n
                               (IPair i marr') <- foldM go (IPair n marr) ws0
                               ba <- unsafeFreezeMutableArray marr'
                               return $! UTF8Slice ba i (sizeofArray ba-i))
  where
    go :: IPair (MutablePrimArray s Word8) -> Char -> ST s (IPair (MutablePrimArray s Word8))
    go (IPair i marr) !c = do
        n <- sizeofMutableArray marr
        let l = encodeCharLength c
        if i >= l
        then do _ <- encodeChar marr (i-l) c
                return (IPair (i-l) marr)
        else do let !n' = n `shiftL` 1  -- double the buffer
                !marr' <- newMutableArray n'
                copyMutableArray marr' (n+i) marr i (n-i)
                let i' = n+i-l
                _ <- encodeChar marr' i' c
                return (IPair i' marr')

-- | /O(n)/ Convert text to a char list.
--
-- Unpacking is done lazily. i.e. we will retain reference to the array until all element are consumed.
--
-- This function is a /good producer/ in the sense of build/foldr fusion.
unpack :: Text -> String
{-# INLINE [1] unpack #-}
unpack (UTF8Slice ba s l) = go s
  where
    !end = s + l
    go !idx
        | idx >= end = []
        | otherwise = let (# c, i #) = decodeChar ba idx in c : go (idx + i)

unpackFB :: Text -> (Char -> a -> a) -> a -> a
{-# INLINE [0] unpackFB #-}
unpackFB (UTF8Slice ba s l) k z = go s
  where
    !end = s + l
    go !idx
        | idx >= end = z
        | otherwise = let (# c, i #) = decodeChar ba idx in c `k` go (idx + i)

{-# RULES
"unpack" [~1] forall t . unpack t = build (\ k z -> unpackFB t k z)
"unpackFB" [1] forall t . unpackFB t (:) [] = unpack t
 #-}

-- | /O(n)/ Convert text to a list in reverse order.
--
-- This function is a /good producer/ in the sense of build/foldr fusion.
unpackR :: Text -> String
{-# INLINE [1] unpackR #-}
unpackR (UTF8Slice ba s l) = go (s+l-1)
  where
    go !idx
        | idx < s = []
        | otherwise = let (# c, i #) = decodeCharReverse ba idx in c : go (idx - i)

unpackRFB :: Text -> (Char -> a -> a) -> a -> a
{-# INLINE [0] unpackRFB #-}
unpackRFB (UTF8Slice ba s l) k z = go (s+l-1)
  where
    go !idx
        | idx < s = z
        | otherwise = let (# c, i #) = decodeCharReverse ba idx in c `k` go (idx - i)

{-# RULES
"unpackR" [~1] forall t . unpackR t = build (\ k z -> unpackRFB t k z)
"unpackRFB" [1] forall t . unpackRFB t (:) [] = unpackR t
 #-}

-- | /O(1)/. Single char text.
singleton :: Char -> Text
{-# INLINE singleton #-}
singleton = T.singleton

-- | /O(1)/. Empty text.
empty :: Text
{-# NOINLINE empty #-}
empty = T.empty

-- | /O(n)/. Copy a text from slice.
copy :: Text -> Text
{-# INLINE copy #-}
copy = T.copy

--------------------------------------------------------------------------------
-- * Basic interface

-- | /O(m+n)/
--
-- There's no need to guard empty vector because we guard them for you, so
-- appending empty text are no-ops.
append :: Text -> Text -> Text
append = T.append
{-# INLINE append #-}

-- | /O(1)/ Test whether a text is empty.
null :: Text -> Bool
{-# INLINE null #-}
null (UTF8Slice _ _ l) = l == 0

-- |  /O(n)/ The char length of a text.
length :: Text -> Int
{-# INLINE length #-}
length (UTF8Slice ba s l) = go s 0
  where
    !end = s + l
    go !i !acc | i >= end = acc
               | otherwise = let j = decodeCharLen ba i in go (i+j) (1+acc)

--------------------------------------------------------------------------------
-- * Transformations
--
-- | /O(n)/ 'map' @f@ @t@ is the 'Text' obtained by applying @f@ to
-- each char of @t@. Performs replacement on invalid scalar values.
map :: (Char -> Char) -> Text -> Text
{-# INLINE map #-}
map f (UTF8Slice arr s l)
    | l == 0 = empty
    | otherwise = runST $ do
        mpa <- newMutableArray (l+3)
        go mpa s 0
  where
    end = s + l
    -- the 3 bytes buffer is here for optimizing ascii mapping
    -- we do resize if less than 4 bytes left when building
    -- to save us from pre-checking encoding char length everytime
    go :: MutablePrimArray s Word8 -> Int -> Int -> ST s Text
    go marr !i !j
        | i >= end = do
            shrinkMutableArray marr j
            arr' <- unsafeFreezeMutableArray marr
            return $! UTF8Slice arr' 0 j
        | otherwise = do
            let (# c, d #) = decodeChar arr i
            j' <- encodeChar marr j (f c)
            let !i' = i + d
            siz <- sizeofMutableArray marr
            if  j' < siz - 3
            then go marr i' j'
            else do
                let !siz' = siz `shiftL` 1
                !marr' <- resizeMutablePrimArray marr siz'
                go marr' i' j' 

-- | Strict mapping with index.
imap :: (Int -> Char -> Char) -> Text -> Text
{-# INLINE imap #-}
imap f (UTF8Slice arr s l)
    | l == 0 = empty
    | otherwise = runST $ do
          mpa <- newMutableArray (l+3)
          go mpa s 0 0
  where
    end = s + l
    go :: MutablePrimArray s Word8 -> Int -> Int -> Int -> ST s Text
    go !marr !i !j !k 
        | i >= end = do
            shrinkMutableArray marr j
            arr' <- unsafeFreezeMutableArray marr
            return $! UTF8Slice arr' 0 j
        | otherwise = do
            let (# c, d #) = decodeChar arr i
            j' <- encodeChar marr j (f k c)
            let !i' = i + d
                !k' = k + 1
            siz <- sizeofMutableArray marr
            if  j' < siz - 3
            then go marr i' j' k' 
            else do
                let !siz' = siz `shiftL` 1
                !marr' <- resizeMutablePrimArray marr siz'
                go marr' i' j' k' 


-- | Shuffle a text using  <https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle Fisher-Yates> algorithm.
shuffle :: StatefulGen g (ST s) => g -> Text -> ST s Text
{-# INLINE shuffle #-}
shuffle g t = fromCharVector <$> V.shuffle g (toCharVector t)

-- | Generate all permutation of a text using <https://en.wikipedia.org/wiki/Heap%27s_algorithm Heap's algorithm>.
permutations :: Text -> [Text]
{-# INLINE permutations #-}
permutations t = fromCharVector <$> V.permutations (toCharVector t)

--------------------------------------------------------------------------------
--
-- Strict folds
--

-- | Strict left to right fold.
foldl' :: (b -> Char -> b) -> b -> Text -> b
{-# INLINE foldl' #-}
foldl' f z (UTF8Slice arr s l) = go z s
  where
    !end = s + l
    -- tail recursive; traverses array left to right
    go !acc !i | i < end  = case decodeChar arr i of
                                (# x, d #) -> go (f acc x) (i + d)
               | otherwise = acc

-- | Strict left to right fold with index.
ifoldl' :: (b -> Int ->  Char -> b) -> b -> Text -> b
{-# INLINE ifoldl' #-}
ifoldl' f z (UTF8Slice arr s l) = go z s 0
  where
    !end = s + l
    go !acc !i !k | i < end  = case decodeChar arr i of
                                    (# x, d #) -> go (f acc k x) (i + d) (k + 1)
                  | otherwise = acc

-- | Strict right to left fold
foldr' :: (Char -> b -> b) -> b -> Text -> b
{-# INLINE foldr' #-}
foldr' f z (UTF8Slice arr s l) = go z (s+l-1)
  where
    -- tail recursive; traverses array right to left
    go !acc !i | i >= s    = case decodeCharReverse arr i of
                                (# x, d #) -> go (f x acc) (i - d)
               | otherwise = acc

-- | /O(n)/ Concatenate a list of text.
--
-- Note: 'concat' have to force the entire list to filter out empty text and calculate
-- the length for allocation.
concat :: [Text] -> Text
concat = T.concat
{-# INLINE concat #-}

-- | /O(n)/ Concatenate a list of text in reverse order, e.g. @concat ["hello, world"] == "worldhello"@
--
-- Note: 'concat' have to force the entire list to filter out empty text and calculate
-- the length for allocation.
concatR :: [Text] -> Text
concatR = T.concat . List.reverse
{-# INLINE concatR #-}

-- | Map a function over a text and concatenate the results
concatMap :: (Char -> Text) -> Text -> Text
{-# INLINE concatMap #-}
concatMap f = concat . foldr' ((:) . f) []

-- | /O(n)/ 'count' returns count of an element from a text.
count :: Char -> Text -> Int
{-# INLINE count #-}
count c (getUTF8Bytes -> v)
    | encodeCharLength c == 1 = let w = c2w c in V.count w v
    | otherwise = let (getUTF8Bytes -> pat) = singleton c
                  in List.length $ V.indices False pat v

-- | /O(n)/ Applied to a predicate and a text, 'any' determines
-- if any chars of the text satisfy the predicate.
any :: (Char -> Bool) -> Text -> Bool
{-# INLINE any #-}
any f (UTF8Slice arr s l)
    | l <= 0    = False
    | otherwise = case decodeChar arr s of
                    (# x0, d #) -> go (f x0) (s+d)
  where
    !end = s+l
    go !acc !i | acc       = True
               | i >= end  = acc
               | otherwise = case decodeChar arr i of
                                (# x, d #) -> go (acc || f x) (i+d)

-- | /O(n)/ Applied to a predicate and text, 'all' determines
-- if all chars of the text satisfy the predicate.
all :: (Char -> Bool) -> Text -> Bool
{-# INLINE all #-}
all f (UTF8Slice arr s l)
    | l <= 0    = True
    | otherwise = case decodeChar arr s of
                    (# x0, d #) -> go (f x0) (s+d)
  where
    !end = s+l
    go !acc !i | not acc   = False
               | i >= end  = acc
               | otherwise = case decodeChar arr i of
                                (# x, d #) -> go (acc && f x) (i+d)

--------------------------------------------------------------------------------
--
-- Building text

-- | /O(n)/ 'replicate' char n time.
--
replicate :: Int -> Char -> Text
{-# INLINE replicate #-}
replicate n c | n <= 0 = empty
              | otherwise = unsafeFromUTF8Bytes (V.create_ siz (go 0))
  where
    !csiz = encodeCharLength c
    !siz = n * csiz
    go :: Int -> MutablePrimArray s Word8 -> ST s ()
    go 0 marr = encodeChar marr 0 c >> go csiz marr
    go i marr | i >= siz = return ()
              | otherwise = do copyChar' csiz marr i marr (i-csiz)
                               go (i+csiz) marr

-- | /O(n*m)/ 'cycle' a text n times.
cycle :: Int -> Text -> Text
{-# INLINE cycle #-}
cycle 0 _        = empty
cycle n (getUTF8Bytes -> v) = unsafeFromUTF8Bytes (V.cycle n v)

--------------------------------------------------------------------------------
-- Convert between codepoint vector and text

-- | /O(n)/ convert from a char vector.
fromCharVector :: V.PrimVector Char -> Text
{-# INLINE fromCharVector #-}
fromCharVector (V.PrimVector arr s l) = unsafeFromUTF8Bytes (V.createN (l*4) (go s 0))
  where
    end = s+l
    go :: forall s. Int -> Int -> MutablePrimArray s Word8 -> ST s Int
    go !i !j !marr
        | i >= end = return j
        | otherwise = do
            let c = indexPrimArray arr i
            j' <- encodeChar marr j c
            go (i+1) j' marr

-- | /O(n)/ convert to a char vector.
toCharVector :: Text -> V.PrimVector Char
{-# INLINE toCharVector #-}
toCharVector (UTF8Slice arr s l) = V.createN l (go s 0)
  where
    end = s+l
    go :: forall s. Int -> Int -> MutablePrimArray s Char -> ST s Int
    go !i !j !marr
        | i >= end = return j
        | otherwise = do
            let (# c, n #) = decodeChar arr i
            writePrimArray marr j c
            go (i+n) (j+1) marr

-- ----------------------------------------------------------------------------
-- ** Normalization
--
-- $normalization

-- | Check if a string is stable in the NFC (Normalization Form C).
isNormalized :: Text -> NormalizationResult
{-# INLINE isNormalized #-}
isNormalized = isNormalizedTo NFC

{-|
Check if a string is stable in the specified Unicode Normalization
Form.

This function can be used as a preprocessing step, before attempting to
normalize a string. Normalization is a very expensive process, it is often
cheaper to first determine if the string is unstable in the requested
normalization form.

The result of the check will be YES if the string is stable and MAYBE or NO
if it is unstable. If the result is MAYBE, the string does not necessarily
have to be normalized.

For more information, please review <http://www.unicode.org/reports/tr15/ Unicode Standard Annex #15 - Unicode
Normalization Forms>.
-}
isNormalizedTo :: NormalizeMode -> Text -> NormalizationResult
isNormalizedTo nmode (UTF8Slice (PrimArray arr#) (I# s#) l@(I# l#))
    | l == 0 = NormalizedYes
    | otherwise =
        let nflag = normalizeModeToFlag nmode
        in toNormalizationResult (z_utf8_isnormalized arr# s# l# nflag)

-- | Normalize a string to NFC (Normalization Form C).
normalize :: Text -> Text
{-# INLINE normalize #-}
normalize = normalizeTo NFC

{-|
Normalize a string to the specified Unicode Normalization Form.

The Unicode standard defines two standards for equivalence between
characters: canonical and compatibility equivalence. Canonically equivalent
characters and sequence represent the same abstract character and must be
rendered with the same appearance and behavior. Compatibility equivalent
characters have a weaker equivalence and may be rendered differently.

Unicode Normalization Forms are formally defined standards that can be used
to test whether any two strings of characters are equivalent to each other.
This equivalence may be canonical or compatibility.

The algorithm puts all combining marks into a specified order and uses the
rules for decomposition and composition to transform the string into one of
four Unicode Normalization Forms. A binary comparison can then be used to
determine equivalence.
-}
normalizeTo :: NormalizeMode -> Text -> Text
normalizeTo nmode (UTF8Slice (PrimArray arr#) (I# s#) l@(I# l#))
    | l == 0 = empty
    | otherwise = unsafeDupablePerformIO $ do
        let nflag = normalizeModeToFlag nmode
            !l'@(I# l'#) = z_utf8_normalize_length arr# s# l# nflag
        when (l' < 0) (error "impossible happened!")
        !pa@(MutablePrimArray marr#) <- newMutableArray l'
        z_utf8_normalize arr# s# l# marr# l'# nflag
        arr' <- unsafeFreezeMutableArray pa
        return $! UTF8Slice arr' 0 l'


-- ----------------------------------------------------------------------------
-- ** Case conversions

-- $case

-- | Remove case distinction from UTF-8 encoded text with default locale.
caseFold :: Text -> Text
caseFold = caseFoldWith LocaleDefault

{-|
Remove case distinction from UTF-8 encoded text.

Case folding is the process of eliminating differences between code points
concerning case mapping. It is most commonly used for comparing strings in a
case-insensitive manner. Conversion is fully compliant with the Unicode 7.0
standard.

Although similar to lowercasing text, there are significant differences.
For one, case folding does _not_ take locale into account when converting.
In some cases, case folding can be up to 20% faster than lowercasing the
same text, but the result cannot be treated as correct lowercased text.

Only two locale-specific exception are made when case folding text.
In Turkish, U+0049 LATIN CAPITAL LETTER I maps to U+0131 LATIN SMALL LETTER
DOTLESS I and U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE maps to U+0069
LATIN SMALL LETTER I.

Although most code points can be case folded without changing length, there are notable
exceptions. For example, U+0130 (LATIN CAPITAL LETTER I WITH DOT ABOVE) maps
to "U+0069 U+0307" (LATIN SMALL LETTER I and COMBINING DOT ABOVE) when
converted to lowercase.

Only a handful of scripts make a distinction between upper- and lowercase.
In addition to modern scripts, such as Latin, Greek, Armenian and Cyrillic,
a few historic or archaic scripts have case. The vast majority of scripts
do not have case distinctions.
-}

caseFoldWith :: Locale -> Text -> Text
caseFoldWith locale (UTF8Slice (PrimArray arr#) (I# s#) l@(I# l#))
    | l == 0 = empty
    | otherwise = unsafeDupablePerformIO $ do
        let !l'@(I# l'#) = z_utf8_casefold_length arr# s# l# locale
        when (l' < 0) (error "impossible happened!")
        !pa@(MutablePrimArray marr#) <- newMutableArray l'
        z_utf8_casefold arr# s# l# marr# l'# locale
        arr' <- unsafeFreezeMutableArray pa
        return $! UTF8Slice arr' 0 l'

-- | Convert UTF-8 encoded text to lowercase with default locale.
toLower :: Text -> Text
toLower = toLowerWith LocaleDefault

{-|
Convert UTF-8 encoded text to lowercase.

This function allows conversion of UTF-8 encoded strings to lowercase
without first changing the encoding to UTF-32. Conversion is fully compliant
with the Unicode 7.0 standard.

Although most code points can be converted to lowercase with changing length,
there are notable exceptions. For example, U+0130 (LATIN CAPITAL LETTER I WITH DOT
ABOVE) maps to "U+0069 U+0307" (LATIN SMALL LETTER I and COMBINING DOT
ABOVE) when converted to lowercase.

Only a handful of scripts make a distinction between upper- and lowercase.
In addition to modern scripts, such as Latin, Greek, Armenian and Cyrillic,
a few historic or archaic scripts have case. The vast majority of scripts do
not have case distinctions.

Case mapping is not reversible. That is, @toUpper(toLower(x)) != toLower(toUpper(x))@.

Certain code points (or combinations of code points) apply rules
based on the locale. For more information about these exceptional
code points, please refer to the Unicode standard:
<ftp://ftp.unicode.org/Public/UNIDATA/SpecialCasing.txt>
-}
toLowerWith :: Locale -> Text -> Text
toLowerWith locale (UTF8Slice (PrimArray arr#) (I# s#) l@(I# l#))
    | l == 0 = empty
    | otherwise = unsafeDupablePerformIO $ do
        let !l'@(I# l'#) = z_utf8_tolower_length arr# s# l# locale
        when (l' < 0) (error "impossible happened!")
        !pa@(MutablePrimArray marr#) <- newMutableArray l'
        z_utf8_tolower arr# s# l# marr# l'# locale
        arr' <- unsafeFreezeMutableArray pa
        return $! UTF8Slice arr' 0 l'

-- | Convert UTF-8 encoded text to uppercase with default locale.
toUpper :: Text -> Text
toUpper = toUpperWith LocaleDefault

{-|
Convert UTF-8 encoded text to uppercase.

Conversion is fully compliant with the Unicode 7.0 standard.

Although most code points can be converted without changing length, there are notable
exceptions. For example, U+00DF (LATIN SMALL LETTER SHARP S) maps to
"U+0053 U+0053" (LATIN CAPITAL LETTER S and LATIN CAPITAL LETTER S) when
converted to uppercase.

Only a handful of scripts make a distinction between upper and lowercase.
In addition to modern scripts, such as Latin, Greek, Armenian and Cyrillic,
a few historic or archaic scripts have case. The vast majority of scripts
do not have case distinctions.

Case mapping is not reversible. That is, @toUpper(toLower(x)) != toLower(toUpper(x))@.

Certain code points (or combinations of code points) apply rules
based on the locale. For more information about these exceptional
code points, please refer to the Unicode standard:
<ftp://ftp.unicode.org/Public/UNIDATA/SpecialCasing.txt>
-}
toUpperWith :: Locale -> Text -> Text
toUpperWith locale (UTF8Slice (PrimArray arr#) (I# s#) l@(I# l#))
    | l == 0 = empty
    | otherwise = unsafeDupablePerformIO $ do
        let !l'@(I# l'#) = z_utf8_toupper_length arr# s# l# locale
        when (l' < 0) (error "impossible happened!")
        !pa@(MutablePrimArray marr#) <- newMutableArray l'
        z_utf8_toupper arr# s# l# marr# l'# locale
        arr' <- unsafeFreezeMutableArray pa
        return $! UTF8Slice arr' 0 l'

-- | Convert UTF-8 encoded text to titlecase with default locale.
toTitle :: Text -> Text
toTitle = toTitleWith LocaleDefault

{-|
Convert UTF-8 encoded text to titlecase.

This function allows conversion of UTF-8 encoded strings to titlecase.
Conversion is fully compliant with the Unicode 7.0 standard.

Titlecase requires a bit more explanation than uppercase and lowercase,
because it is not a common text transformation. Titlecase uses uppercase
for the first letter of each word and lowercase for the rest. Words are
defined as "collections of code points with general category Lu, Ll, Lt, Lm
or Lo according to the Unicode database".

Effectively, any type of punctuation can break up a word, even if this is
not grammatically valid. This happens because the titlecasing algorithm
does not and cannot take grammar rules into account.

@
Text                                 | Titlecase
-------------------------------------|-------------------------------------
The running man                      | The Running Man
NATO Alliance                        | Nato Alliance
You're amazing at building libraries | You'Re Amazing At Building Libraries
@

Although most code points can be converted to titlecase without changing length,
there are notable exceptions. For example, U+00DF (LATIN SMALL LETTER SHARP S) maps to
"U+0053 U+0073" (LATIN CAPITAL LETTER S and LATIN SMALL LETTER S) when
converted to titlecase.

Certain code points (or combinations of code points) apply rules
based on the locale. For more information about these exceptional
code points, please refer to the Unicode standard:
<ftp://ftp.unicode.org/Public/UNIDATA/SpecialCasing.txt>
-}

toTitleWith :: Locale -> Text -> Text
toTitleWith locale (UTF8Slice (PrimArray arr#) (I# s#) l@(I# l#))
    | l == 0 = empty
    | otherwise = unsafeDupablePerformIO $ do
        let !l'@(I# l'#) = z_utf8_totitle_length arr# s# l# locale
        when (l' < 0) (error "impossible happened!")
        !pa@(MutablePrimArray marr#) <- newMutableArray l'
        z_utf8_totitle arr# s# l# marr# l'# locale
        arr' <- unsafeFreezeMutableArray pa
        return $! UTF8Slice arr' 0 l'


{-|
Check if the input string conforms to the category specified by the
flags.

This function can be used to check if the code points in a string are part
of a category. Valid flags are members of the "list of categories".
The category for a code point is defined as part of the entry in UnicodeData.txt,
the data file for the Unicode code point database.

By default, the function will treat grapheme clusters as a single code
point. This means that the following string:

@
Code point | Canonical combining class | General category      | Name
---------- | ------------------------- | --------------------- | ----------------------
U+0045     | 0                         | Lu (Uppercase letter) | LATIN CAPITAL LETTER E
U+0300     | 230                       | Mn (Non-spacing mark) | COMBINING GRAVE ACCENT
@

Will match with 'CategoryLetterUppercase' in its entirety, because
the COMBINING GRAVE ACCENT is treated as part of the grapheme cluster. This
is useful when e.g. creating a text parser, because you do not have to
normalize the text first.

If this is undesired behavior, specify the 'CategoryIgnoreGraphemeCluster' flag.

In order to maintain backwards compatibility with POSIX functions
like `isdigit` and `isspace`, compatibility flags have been provided. Note,
however, that the result is only guaranteed to be correct for code points
in the Basic Latin range, between U+0000 and 0+007F. Combining a
compatibility flag with a regular category flag will result in undefined
behavior.
-}

isCategory :: Category -> Text -> Bool
{-# INLINE isCategory #-}
isCategory c (UTF8Slice (PrimArray arr#) (I# s#) l@(I# l#))
    | l == 0 = True
    | otherwise = z_utf8_iscategory arr# s# l# c == l


{-|
Try to match as many code points with the matching category flags as possible
and return the prefix and suffix.
-}
spanCategory :: Category -> Text -> (Text, Text)
{-# INLINE spanCategory #-}
spanCategory c (UTF8Slice arr@(PrimArray arr#) s@(I# s#) l@(I# l#))
    | l == 0 = (empty, empty)
    | otherwise =
        let i = z_utf8_iscategory arr# s# l# c
        in (UTF8Slice arr s i, UTF8Slice arr (s+i) (l-i))

-- | Get the display width of a piece of text.
--
-- You shouldn't pass texts with control characters(<0x20, \\DEL), which are counted with -1 width.
--
-- >>> displayWidth "你好世界！"
-- 10
-- >>> displayWidth "hello world!"
-- 12
displayWidth :: Text -> Int
{-# INLINE displayWidth #-}
displayWidth (UTF8Slice ba s l) = go s 0
  where
    !end = s + l
    go !i !acc
        | i >= end = acc
        | otherwise =
            let (# c, n #) = decodeChar ba i
            in go (i+n) (acc + displayWidthChar c)

-- | Get the display width of a 'Char'.
--
-- You shouldn't pass texts with control characters(<0x20, \\DEL), which are counted with -1 width.
displayWidthChar :: Char -> Int
{-# INLINE displayWidthChar #-}
displayWidthChar c =  z_wcwidth (fromIntegral (fromEnum c))


-- | Compare two 'Text's with <https://www.unicode.org/reports/tr10/ Unicode Collation Algorithm>
collate :: Collator -> Text -> Text -> Ordering
{-# INLINE collate #-}
collate cltr = collateWithUnpacker cltr unpack
