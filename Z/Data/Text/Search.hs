{-|
Module      : Z.Data.Text.Search
Description : Searching text
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

-}

module Z.Data.Text.Search (
  -- * element-wise search
    elem, notElem
  -- * Searching by equality
  , findIndices
  , findBytesIndices
  , find, findR
  , findIndex
  , findIndexR
  , findBytesIndex
  , findBytesIndexR
  , filter, partition
  ) where


import           Control.Monad.ST
import           Data.Word
import           Prelude                 hiding (elem, notElem, filter)
import           Z.Data.Array
import           Z.Data.Text.Base
import           Z.Data.Text.UTF8Codec
import qualified Z.Data.Vector.Base    as V

-- | find all char index matching the predicate.
findIndices :: (Char -> Bool) -> Text -> [Int]
{-# INLINABLE findIndices #-}
findIndices f (Text (V.PrimVector arr s l)) = go 0 s
  where
    !end = s + l
    go !i !p | p >= end  = []
             | f x       = i : go (i+1) (p+off)
             | otherwise = go (i+1) (p+off)
        where (# x, off #) = decodeChar arr p

-- | find all char's byte index matching the predicate.
findBytesIndices :: (Char -> Bool) -> Text -> [Int]
{-# INLINABLE findBytesIndices #-}
findBytesIndices f (Text (V.PrimVector arr s l)) = go s
  where
    !end = s + l
    go !p | p >= end  = []
          | f x       = (p-s) : go (p+off)
          | otherwise = go (p+off)
        where (# x, off #) = decodeChar arr p

-- | /O(n)/ find the first char matching the predicate in a text
-- from left to right, if there isn't one, return the text length.
find :: (Char -> Bool)
     -> Text
     -> (Int, Maybe Char)  -- ^ (char index, matching char)
{-# INLINABLE find #-}
find f (Text (V.PrimVector arr s l)) = go 0 s
  where
    !end = s + l
    go !i !j | j >= end  = (i, Nothing)
             | otherwise =
                let (# x, off #) = decodeChar arr j
                in if f x
                    then (i, Just x)
                    else go (i+1) (j+off)

-- | /O(n)/ find the first char matching the predicate in a text
-- from right to left.
--
findR :: (Char -> Bool)
      -> Text
      -> (Int, Maybe Char)  -- ^ (char index(counting backwards), matching char)
{-# INLINABLE findR #-}
findR f (Text (V.PrimVector arr s l)) = go 0 (s+l-1)
  where
    go !i !j | j < s     = (i, Nothing)
             | otherwise =
                let (# x, off #) = decodeCharReverse arr j
                in if f x
                    then (i, Just x)
                    else go (i+1) (j-off)

--------------------------------------------------------------------------------

-- | /O(n)/ find the char index.
findIndex :: (Char -> Bool) -> Text -> Int
{-# INLINABLE findIndex #-}
findIndex f t = case find f t of (i, _) -> i

-- | /O(n)/ find the char index in reverse order.
findIndexR ::  (Char -> Bool) -> Text -> Int
{-# INLINABLE findIndexR #-}
findIndexR f t = case findR f t of (i, _) -> i

-- | /O(n)/ find the char's byte slice index.
findBytesIndex :: (Char -> Bool) -> Text -> Int
{-# INLINABLE findBytesIndex #-}
findBytesIndex f (Text (V.PrimVector arr s l)) = go s
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
findBytesIndexR f (Text (V.PrimVector arr s l)) = go (s+l-1)
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
filter f (Text (V.PrimVector arr s l)) = Text (V.createN l (go s 0))
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
partition f (Text (V.PrimVector arr s l))
    | l == 0    = (empty, empty)
    | otherwise = let !(bs1, bs2) = V.createN2 l l (go 0 0 s) in (Text bs1, Text bs2)
  where
    !end = s + l
    go :: Int -> Int -> Int -> MutablePrimArray s Word8 -> MutablePrimArray s Word8 -> ST s (Int, Int)
    go !i !j !p !mba0 !mba1
        | p >= end   = return (i, j)
        | otherwise =
            let (# x, off #) = decodeChar arr p
            in if f x
                then copyChar off mba0 i arr p >> go (i+off) j (p+off) mba0 mba1
                else copyChar off mba1 j arr p >> go i (j+off) (p+off) mba0 mba1

--------------------------------------------------------------------------------
-- Searching by equality

-- | /O(n)/ 'elem' test if given char is in given text.
elem :: Char -> Text -> Bool
{-# INLINABLE elem #-}
elem x t = case find (x==) t of (_,Nothing) -> False
                                _           -> True

-- | /O(n)/ @not . elem@
notElem ::  Char -> Text -> Bool
{-# INLINABLE notElem #-}
notElem x = not . elem x
