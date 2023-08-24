{-|
Module      : Z.Vector
#ifdef CN_DOC
Description : 一维向量（数组切片）
#else
Description : Fast boxed and unboxed vector
#endif
Copyright   : (c) Dong Han, 2017-2023
              (c) Tao He, 2018-2023
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

#ifdef CN_DOC
本模块提供统一的函数来操作各类数组切片，其中 'Vector' 用了存放盒装数据，'PrimVector' 用来存放
'Prim' 实例，顾名思义切片就是一个数组加上起始下标和长度，你可以用 'Slice' 来匹配或者构造切片内部。

本模块需要接受下标的函数都接收负数下标，也就是从末尾（@-1@）开始计算的下标。数组类型本身也是切片实例，
不过数组的切片操作由于涉及拷贝往往不是/O(1)/的。
#else
This module provides unified vector interface similar to @vector@ or @bytestrings@.
Conceptually vectors are just slices of array, and you can use 'Slice' pattern to
match or build the slice like @ case v of Slice arr off len -> ... Slice arr off' len' .. @.
In this module two concrete vector types are provided:
'Vector' for holding boxed values, and 'PrimVector' for holding prim elements.

Negative index, or indexing from rear is supported in all functions take numeric indices,
such as 'index' or 'slice'. The rule is count down from @-1@ when indexing from rear.
Array types are also vectors, but they may not always provide O(1) slicing: 
The slice range is always from 0 to its length, and slicing will make copy if neccessary.
#endif

==== __Examples__
>>> import qualified Z.Data.Vector as V
>>> import           Z.Data.Vector ((!!), (!:), (!/))
>>> V.pack [1..10] :: PrimVector Int  -- vector can be create by 'pack' a list
[1,2,3,4,5,6,7,8,9,10]
>>> :set -XOverloadedLists            
>>> let v = [1..10] :: PrimVector Int  -- OverloadedList will automatically add 'pack'
>>> V.index' v 0        -- indexing
1
>>> V.index' v (-1)     -- indexing from end
10
>>> V.take 3 v          -- slice to index
[1,2,3]
>>> V.take (-3) v       -- slice to reverse index
[1,2,3,4,5,6,7]
>>> :set -XOverloadedStrings
>>> "helloworld" :: Bytes OverloadedString can be used with ASCII chars to create Bytes
"helloworld"
>>> slice 1 (-1) "helloworld"
"elloworl"
>>> "helloworld" !: (1, -1)  -- infix slice
"elloworl"
>>> "helloworld" !: 3   -- infix splitAt
("hel","loworld")
>>> "helloworld" !! 1   -- infix index
104
-}

module Z.Data.Vector (
  -- * Vectors & Instances
    Vectors(..)
  , pattern Slice
  , Vector(..)
  , PrimVector(..)
  , Bytes
  -- * Conversion between list
  , packASCII, pack, packN, packR, packRN
  , unpack, unpackR, iunpack, iunpackR
  -- * Basic interface
  , index, (!?), index', (!!), slice, (!:), splitAt, (!/), take, drop
  , cons, snoc, uncons, unsnoc, inits, tails
  , null, length, append, concat, concatR, concatMap
  , shuffle, permutations, reverse, intersperse, intercalate, intercalateElem, transpose
  , takeWhile, takeWhileR, dropWhile, dropWhileR, dropAround
  , filter, partition, break, span, breakR, spanR, breakOn
  , group, groupBy, split, splitWith, splitOn, replace
  , isPrefixOf, isSuffixOf, isInfixOf, commonPrefix
  , stripPrefix, stripSuffix
  , words, lines, unwords, unlines
  , padLeft, padRight
  -- * Element-wise modified copy
  , modifyAt, modifyAt', insertAt, insertAt', deleteAt, deleteAt'
  -- * Vectors creation utilities
  , empty, singleton, doubleton, tripleton, copy
  , replicate, replicateM, cycle
  , scanl, scanl1, scanr, scanr1
  , create, create_, createN 
  -- * Map, Fold, Traverse
  , map, imap
  , foldl', ifoldl', foldl1', foldr'
  , mapAccumL, mapAccumR, imapAccumL, imapAccumR
  , maximum, minimum
  , sum, count, product, all, any
  , traverse, itraverse, traverse_, itraverse_
  , mapM, mapM_, forM, forM_, imapM, imapM_, iforM, iforM_
  , zipWith, unzipWith
  -- * Search element and sub-slice
  , elem, notElem, elemIndices, findElem, findElemR, findAllElem
  , indices, indicesOverlapping
  -- * Merge sort and radix sort
  , mergeSort
  , mergeSortBy
  , Down(..)
  , radixSort
  , Radix(..)
  , RadixDown(..)
  , mergeAdjacent
  , mergeAdjacentBy
  -- * Misc
  , eqVector, compareVector
  , IPair(..), fromIPair, toIPair
  , defaultInitSize
  , chunkOverhead
  , defaultChunkSize
  , smallChunkSize
  , VectorException(..)
  , errorEmptyVector
  , errorOutOfRange
  , replicatePM
  , itraversePM
  , kmpNextTable
  , sundayBloom
  , elemSundayBloom
 ) where

import           Control.DeepSeq
import           Control.Exception
import qualified Control.Monad             as M
import           Control.Monad.ST
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.CaseInsensitive      as CI
import           Data.Char                 (ord)
import qualified Data.Foldable             as F
import           Data.Functor.Classes      (Eq1(..), Ord1(..))
import           Data.Hashable             (Hashable (..))
import           Data.Hashable.Lifted      (Hashable1 (..), hashWithSalt1)
import           Data.Int
import           Data.Kind                 (Type)
import qualified Data.List                 as List
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import           Data.Maybe
import           Data.Semigroup            (Semigroup (..))
import qualified Data.Traversable          as T
import           GHC.Exts
import           GHC.Stack
import           GHC.Word
import           Prelude                   hiding ((!!), all, any, concat, concatMap, cycle,
                                            elem, foldl, foldl1, foldr, foldr1,
                                            length, map, mapM, mapM_, null,
                                            elem, notElem, null, length, map,
                                            foldl, foldl1, foldr, foldr1,
                                            maximum, minimum, product, sum,
                                            all, any, replicate, traverse, 
                                            scanl, scanl1, scanr, scanr1,
                                            head, tail, init, last,
                                            take, drop, splitAt,
                                            takeWhile, dropWhile,
                                            break, span, reverse, filter,
                                            words, lines, unwords, unlines, zipWith)

import           System.IO.Unsafe          (unsafeDupablePerformIO)
import           System.Random.Stateful    (StatefulGen)
import           Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import           Test.QuickCheck.Gen       (chooseInt)
import           Text.Read                 (Read (..))

import           Z.Data.Array
import           Z.Data.ASCII              (w2c, c2w, toLower, isSpace)
import           Z.Data.Utils.Cast
import           Z.Data.Utils.FFI          (c_strlen, z_count, z_memchr, z_memrchr, z_fnv_hash, z_intersperse)
import           Z.Monad.STE

-- $setup
-- >>> import Test.QuickCheck.Modifiers
-- >>> import Test.QuickCheck.Function
-- >>> :set -XOverloadedLists

#ifdef CN_DOC
-- | 可以表示成数组切片的类型类。
#else
-- | Typeclass for box and unboxed array slices.
#endif
class (Arrays (BaseArray v) a) => Vectors v a where
#ifdef CN_DOC
    -- | 被切片的数组类型。
#else
    -- | Vector's immutable array type
#endif
    type BaseArray v :: Type -> Type
#ifdef CN_DOC
    -- | 获取数组和切片范围（起始下标和切片长度）
#else
    -- | Get underline array and slice range(start and length).
#endif
    toSlice :: v a -> (# BaseArray v a, Int, Int #)
#ifdef CN_DOC
    -- | 通过数组和切片范围（起始下标和切片长度）生产 Vector。
#else
    -- | Create a vector by slicing an array(with start and length).
#endif
    fromSlice :: BaseArray v a -> Int -> Int -> v a

#ifdef CN_DOC
-- | 用来匹配切片内容的模式别名，@ case v of Slice arr off len -> ...@
-- 你也可以用它来创建切片，但请保证提供的切片范围是合法的:
--
-- * 起始下标和长度都落在数组范围内的。
-- * 长度大于等于0。
#else
-- | A pattern synonyms for matching the underline array, offset and length: 
-- @ case v of Slice arr off len -> ...@.
-- This is a bidirectional pattern synonyms, but very unsafe if not use properly.
-- Make sure your slice is within array's bounds!
#endif
pattern Slice :: Vectors v a => BaseArray v a -> Int -> Int -> v a
pattern Slice arr s l <- (toSlice -> (# arr,s,l #)) where
        Slice arr s l = fromSlice arr s l
{-# COMPLETE Slice #-}

#define ARRAY_INSTANCES(T) \
    type BaseArray T = T; \
    toSlice arr = (# arr, 0, sizeofArray arr #); \
    {-# INLINE toSlice #-}; \
    fromSlice = sliceArray; \
    {-# INLINE fromSlice #-}

instance Vectors Array a where ARRAY_INSTANCES(Array)
instance Vectors SmallArray a where ARRAY_INSTANCES(SmallArray)
instance Prim a => Vectors PrimArray a where ARRAY_INSTANCES(PrimArray)
instance PrimUnlifted a => Vectors UnliftedArray a where ARRAY_INSTANCES(UnliftedArray)
instance PrimUnlifted a => Vectors UnliftedSmallArray a where ARRAY_INSTANCES(UnliftedSmallArray)

--------------------------------------------------------------------------------
#ifdef CN_DOC
-- | 盒装数组切片
#else
-- | Boxed vector
#endif
data Vector a = Vector
    {-# UNPACK #-} !(SmallArray a)  -- ^ payload
    {-# UNPACK #-} !Int             -- ^ start offset
    {-# UNPACK #-} !Int             -- ^ end offset

instance Vectors Vector a where
    type BaseArray Vector = SmallArray
    {-# INLINE toSlice #-}
    toSlice (Vector arr s l) = (# arr, s, l #)
    {-# INLINE fromSlice #-}
    fromSlice = Vector

instance IsList (Vector a) where
    type Item (Vector a) = a
    {-# INLINE fromList #-}
    fromList = pack
    {-# INLINE toList #-}
    toList = unpack
    {-# INLINE fromListN #-}
    fromListN = packN

instance Eq a => Eq (Vector a) where
    {-# INLINE (==) #-}
    v1 == v2 = eqVector (==) v1 v2

instance Eq1 Vector where
    {-# INLINE liftEq #-}
    liftEq = eqVector 

#ifdef CN_DOC
-- | 使用自定义的元素比较函数比较数组切片是否相等。
#else
-- | Compare vectors equality based on user provide element comparing function.
#endif
eqVector :: (a -> b -> Bool) -> Vector a -> Vector b -> Bool
{-# INLINE eqVector #-}
eqVector eq (Slice baA sA lA) (Slice baB sB lB) =
    lA == lB && (sameStart || go sA sB)
  where
    sameStart = baA `sameArray` (unsafeCoerce# baB) && sA == sB
    eA = sA + lA
    go !i !j
        | i >= eA = True
        | otherwise =
            inline eq (indexArray baA i) (indexArray baB j) && go (i+1) (j+1)
-- $property
-- prop> (as == (bs :: Vector Integer)) == (unpack as == unpack bs)

instance Ord a => Ord (Vector a) where
    {-# INLINE compare #-}
    compare = compareVector compare

instance Ord1 Vector where
    {-# INLINE liftCompare #-}
    liftCompare = compareVector

#ifdef CN_DOC
-- | 使用自定义的元素比较函数比较数组切片。
#else
-- | Compare vectors based on user provide element comparing function.
#endif
compareVector :: (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
{-# INLINE compareVector #-}
compareVector cmp (Slice baA sA lA) (Slice baB sB lB)
    | sameStart = lA `compare` lB
    | otherwise = go sA sB
  where
    eA = sA + lA
    eB = sB + lB
    sameStart = baA `sameArray` (unsafeCoerce# baB) && sA == sB
    go !i !j | i >= eA = j `compare` eB
             | j >= eB = GT
             | otherwise =
                case inline cmp (indexArray baA i) (indexArray baB j) of
                    EQ -> go (i+1) (j+1)
                    x  -> x
-- $property
-- prop> as `compare` (bs :: Vector Integer) == (unpack as) `compare` (unpack bs)

instance Semigroup (Vector a) where
    {-# INLINE (<>) #-}
    (<>)    = append
    {-# INLINE sconcat #-}
    sconcat (b:|bs) = concat (b:bs)
    {-# INLINE stimes #-}
    stimes  = cycle . fromIntegral

instance Monoid (Vector a) where
    {-# INLINE mempty #-}
    mempty  = empty
    {-# INLINE mappend #-}
    mappend = (<>)
    {-# INLINE mconcat #-}
    mconcat = concat

instance NFData a => NFData (Vector a) where
    {-# INLINE rnf #-}
    rnf (Vector arr s l) = go s
      where
        e = s + l
        go !i | i < e  = case indexArray arr i of x -> x `seq` go (i+1)
              | otherwise = ()

instance (Show a) => Show (Vector a) where
    showsPrec p v = showsPrec p (unpack v)

instance (Read a) => Read (Vector a) where
    readPrec = pack <$> readPrec

instance Functor Vector where
    {-# INLINE fmap #-}
    fmap = map

instance F.Foldable Vector where
    {-# INLINE foldr' #-}
    foldr' = foldr'
    {-# INLINE foldr #-}
    foldr f acc = List.foldr f acc . unpack
    {-# INLINE foldl' #-}
    foldl' = foldl'
    {-# INLINE foldl #-}
    foldl f acc = List.foldr (flip f) acc . unpackR
    {-# INLINE toList #-}
    toList = unpack
    {-# INLINE null #-}
    null = null
    {-# INLINE length #-}
    length = length
    {-# INLINE elem #-}
    elem = elem
    {-# INLINE maximum #-}
    maximum = maximum
    {-# INLINE minimum #-}
    minimum = minimum
    {-# INLINE product #-}
    product = product
    {-# INLINE sum #-}
    sum = sum

instance T.Traversable Vector where
    {-# INLINE traverse #-}
    traverse = traverse

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = do
        vs <- arbitrary
        let l = List.length vs
        s <- chooseInt (0, l)
        e <- chooseInt (s, l)
        pure $! Slice (pack vs) s (e-s)
    shrink v = pack <$> shrink (unpack v)

instance CoArbitrary a => CoArbitrary (Vector a) where
    coarbitrary = coarbitrary . unpack

instance Hashable a => Hashable (Vector a) where
    {-# INLINE hashWithSalt #-}
    hashWithSalt = hashWithSalt1

instance Hashable1 Vector where
    {-# INLINE liftHashWithSalt #-}
    liftHashWithSalt h salt0 (Vector arr s l) = hashWithSalt (go salt0 s) l
      where
        e = s + l
        go !salt !i
            | i >= e    = salt
            | otherwise = go (h salt (indexArray arr i)) (i+1)

--------------------------------------------------------------------------------
#ifdef CN_DOC
-- | 原始类型数组（元素必须是'Prim'类型类的实例）。
#else
-- | Primitive vector for 'Prim' instances.
#endif
data PrimVector a = PrimVector
    {-# UNPACK #-} !(PrimArray a)   -- ^ payload
    {-# UNPACK #-} !Int             -- ^ offset in elements of type a rather than in bytes
    {-# UNPACK #-} !Int             -- ^ length in elements of type a rather than in bytes

instance Prim a => Vectors PrimVector a where
    type BaseArray PrimVector = PrimArray
    {-# INLINE toSlice #-}
    toSlice (PrimVector arr s l) = (# arr, s, l #)
    {-# INLINE fromSlice #-}
    fromSlice = PrimVector

instance (Prim a, Eq a) => Eq (PrimVector a) where
    {-# INLINE (==) #-}
    (==) = eqPrimVector

-- prop> \ (as == bs) == (unpack as == unpack bs)
eqPrimVector :: forall a. Prim a => PrimVector a -> PrimVector a -> Bool
{-# INLINE eqPrimVector #-}
eqPrimVector (PrimVector (PrimArray baA#) (I# sA#) lA)
             (PrimVector (PrimArray baB#) (I# sB#) lB)
    = -- we use memcmp for all primitive vector, ghc emit code to test
      -- pointer equality so we don't have to do it manually here
      lA == lB &&
        0 == I# (compareByteArrays# baA# (sA# *# siz#) baB# (sB# *# siz#) n#)
  where
    !siz@(I# siz#) = sizeOf (undefined :: a)
    !(I# n#) = lA*siz

instance (Prim a, Ord a) => Ord (PrimVector a) where
    {-# INLINE compare #-}
    compare = comparePrimVector
-- $property
-- prop> (as == (bs :: PrimVector Int)) == (unpack as == unpack bs)
-- prop> (as == (bs :: Bytes)) == (unpack as == unpack bs)

comparePrimVector :: (Prim a, Ord a) => PrimVector a -> PrimVector a -> Ordering
{-# INLINE comparePrimVector #-}
comparePrimVector (PrimVector baA sA lA) (PrimVector baB sB lB)
    | baA `sameArray` baB = if sA == sB then lA `compare` lB else go sA sB
    | otherwise = go sA sB
  where
    eA = sA + lA
    eB = sB + lB
    go !i !j | i >= eA  = j `compare` eB
             | j >= eB  = GT
             | otherwise = let o = indexArray baA i `compare` indexArray baB j
                           in case o of EQ -> go (i+1) (j+1)
                                        x  -> x
-- $property
-- prop> as `compare` (bs :: Bytes) == unpack as `compare` unpack bs
-- prop> as `compare` (bs :: PrimVector Int) == unpack as `compare` unpack bs

#ifdef CN_DOC
-- | 这是一个 <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html INCOHERENT> 实例,
-- 使用 'compareByteArrays#' 来加速比较。
#else
-- | This is an INCOHERENT instance, compare binary data using 'compareByteArrays#' prim-ops.
#endif
instance {-# INCOHERENT #-} Ord Bytes where
    {-# INLINE compare #-}
    compare = compareBytes

compareBytes :: PrimVector Word8 -> PrimVector Word8 -> Ordering
{-# INLINE compareBytes #-}
compareBytes (PrimVector (PrimArray baA#) (I# sA#) lA)
             (PrimVector (PrimArray baB#) (I# sB#) lB) =
    let !(I# n#) = min lA lB
        r = I# (compareByteArrays# baA# sA# baB# sB# n#)
    in case r `compare` 0 of
        EQ -> lA `compare` lB
        x  -> x
-- $property
-- prop> xs `compareBytes` ys == xs `comparePrimVector` ys

instance Prim a => Semigroup (PrimVector a) where
    {-# INLINE (<>) #-}
    (<>)    = append
    {-# INLINE sconcat #-}
    sconcat (b:|bs) = concat (b:bs)
    {-# INLINE stimes #-}
    stimes  = cycle . fromIntegral

instance Prim a => Monoid (PrimVector a) where
    {-# INLINE mempty #-}
    mempty  = empty
    {-# INLINE mappend #-}
    mappend = (<>)
    {-# INLINE mconcat #-}
    mconcat = concat

instance NFData (PrimVector a) where
    {-# INLINE rnf #-}
    rnf PrimVector{} = ()

instance (Prim a, Show a) => Show (PrimVector a) where
    showsPrec p v = showsPrec p (unpack v)

instance (Prim a, Read a) => Read (PrimVector a) where
    readPrec = pack <$> readPrec

instance (Prim a, Arbitrary a) => Arbitrary (PrimVector a) where
    arbitrary = do
        vs <- arbitrary
        let l = List.length vs
        s <- chooseInt (0, l)
        e <- chooseInt (s, l)
        pure $! Slice (pack vs) s (e-s)
    shrink v = pack <$> shrink (unpack v)

instance (Prim a, CoArbitrary a) => CoArbitrary (PrimVector a) where
    coarbitrary = coarbitrary . unpack

instance (Hashable a, Prim a) => Hashable (PrimVector a) where
    {-# INLINE hashWithSalt #-}
    hashWithSalt = hashWithSaltPrimVector

hashWithSaltPrimVector :: (Hashable a, Prim a) => Int -> PrimVector a -> Int
{-# INLINE hashWithSaltPrimVector #-}
hashWithSaltPrimVector salt0 (PrimVector arr s l) = hashWithSalt (go salt0 s) l
  where
    -- we don't do a final hash with length to keep consistent with Bytes's instance
    e = s + l
    go !salt !i
        | i >= e    = salt
        | otherwise = go (hashWithSalt salt (indexArray arr i)) (i+1)

instance Cast a b => Cast (PrimVector a) (PrimVector b) where
    {-# INLINE cast #-}
    cast = unsafeCoerce#

--------------------------------------------------------------------------------

#ifdef CN_DOC
-- | 'Bytes' 是 'PrimVector' 'Word8' 的类型别名。
#else
-- | 'Bytes' is just primitive word8 vectors.
#endif
type Bytes = PrimVector Word8

#ifdef CN_DOC
-- | 这是一个 <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html INCOHERENT> 实例，
-- 使用 FNV-1a 而不是像 @Vector Word8@ 或者 @[Word8]@ 那样使用 FNV-1。
#else
-- | This is an INCOHERENT instance, using FNV-1a which is different from
-- @Vector Word8@ or @[Word8]@ which use FNV-1.
#endif
instance {-# INCOHERENT #-} Hashable Bytes where
    {-# INLINE hashWithSalt #-}
    hashWithSalt = hashWithSaltBytes

hashWithSaltBytes :: Int -> Bytes -> Int
{-# INLINE hashWithSaltBytes #-}
hashWithSaltBytes salt (PrimVector (PrimArray ba#) s l) =
    unsafeDupablePerformIO (z_fnv_hash ba# s l salt)

#ifdef CN_DOC
-- | 这是一个 <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html INCOHERENT> 实例，
-- 提供类似字符串的输出。
#else
-- | This is an INCOHERENT instance, provide 'Bytes' string like output.
#endif
instance {-# INCOHERENT #-} Show Bytes where
    showsPrec p bs r = showsPrec p (List.map w2c (unpack bs)) r

{-|
#ifdef CN_DOC
这是一个 <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html INCOHERENT> 实例，
接受类似数组或者字符串的语法输入。
#else
This is an INCOHERENT instance, accept string like or array like input.
#endif
>>> read "[1,2,3,4]" :: Bytes
"\SOH\STX\ETX\EOT"
>>> read "\"hello world\"" :: Bytes
"hello world"
-}
instance {-# INCOHERENT #-} Read Bytes where
    readsPrec p str@(h:_)
        | h == '[' = [ (pack x, y) | (x, y) <- readsPrec p str ]
        | h == '"' = [ (pack (List.map c2w x), y) | (x, y) <- readsPrec p str ]
        | otherwise = []
    readsPrec _ _ = []

{-|
#ifdef CN_DOC
这个实例使用 'packASCII' 来转化字符串到数组，所以会把UTF8字符的高位忽略！所以请配合 ASCII 字符使用。
#else
This instance use 'packASCII', which may silently chop bytes, use it with ASCII literals only.
#endif
-}
instance IsString Bytes where
    {-# INLINE fromString #-}
    fromString = packASCII

instance Prim a => IsList (PrimVector a) where
    type Item (PrimVector a) = a
    {-# INLINE fromList #-}
    fromList = pack
    {-# INLINE toList #-}
    toList = unpack
    {-# INLINE fromListN #-}
    fromListN = packN

#ifdef CN_DOC
-- | 这个实例假定使用 ASCII 编码
#else
-- | This instance assume ASCII encoded bytes
#endif
instance CI.FoldCase Bytes where
    {-# INLINE foldCase #-}
    foldCase = map toLower

{-|
#ifdef CN_DOC
/O(n)/, 打包 ASCII 编码 'String' 到 'Bytes'，非 ASCII 字符的高位会被忽略。
#else
/O(n)/, pack an ASCII 'String', multi-bytes char WILL BE CHOPPED!
#endif
>>> "你好" :: Bytes -- high bits will be chopped!
"`}"
-}
packASCII :: String -> Bytes
{-# INLINE CONLIKE [0] packASCII #-}
{-# RULES "packASCII/packASCIIAddr" forall addr . packASCII (unpackCString# addr) = packASCIIAddr addr #-}
packASCII = pack . fmap (fromIntegral . ord)

packASCIIAddr :: Addr# -> Bytes
{-# INLINE packASCIIAddr #-}
packASCIIAddr addr0# = go addr0#
  where
    len = fromIntegral . unsafeDupablePerformIO $ c_strlen addr0#
    go addr# = runST $ do
        marr <- newMutableArray len
        copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
        arr <- unsafeFreezeMutableArray marr
        return (PrimVector arr 0 len)

--------------------------------------------------------------------------------
-- Indexing & Slicing

{-|
#ifdef CN_DOC 
取下标对应元素，支持负数下标（等价于 @len + i@），超过范围返回 'Nothing'。
#else
/O(1)/ Index array element, negative index is supported(equivalent to @len + i@),
Return 'Nothing' if index outside of the vector. 
#endif
>>> ([2,4..10] :: PrimVector Int) `index` 3
Just 8
>>> ([2,4..10] :: PrimVector Int) `index` (-1)
Just 10
>>> ([2,4..10] :: PrimVector Int) `index` 10
Nothing
-}
index :: Vectors v a => v a -> Int -> Maybe a
{-# INLINE index #-}
index (Slice arr s l) i 
    | i < 0     = if i >= -l then arr `indexArrayM` (s + l + i) else Nothing
    | otherwise = if i < l   then arr `indexArrayM` (s + i) else Nothing

{-|
#ifdef CN_DOC
中缀版本 'index'
#else
Infix 'index'
#endif
>>> ([2,4..10] :: PrimVector Int) !? 3
Just 8
>>> ("hello world" :: Bytes) !? 3
Just 108
>>> ([2,4..10] :: PrimVector Int) !? 10
Nothing
-}
(!?) :: (Vectors v a, HasCallStack) => v a -> Int -> Maybe a
(!?) = index


{-|
#ifdef CN_DOC
取下标对应元素，支持负数下标（等价于 @len + i@），超过范围报错 'IndexOutOfVectorRange'。
#else
/O(1)/ Index array element, negative index is supported(equivalent to @len + i@),
Throw 'IndexOutOfVectorRange' if index outside of the vector.
#endif
>>> ([2,4..10] :: PrimVector Int) `index'` 3
8
>>> ("hello world" :: Bytes) `index'` 3
108
>>> ([2,4..10] :: PrimVector Int) `index'` 10
*** Exception: IndexOutOfVectorRange...
-}
index' :: (Vectors v a, HasCallStack) => v a -> Int -> a
{-# INLINE index' #-}
index' (Slice arr s l) i 
    | i < 0     = if i >= -l then arr `indexArray` (s + l + i) else errorOutOfRange i
    | otherwise = if i < l   then arr `indexArray` (s + i) else errorOutOfRange i

{-|
#ifdef CN_DOC
中缀版本 'index''
#else
Infix 'index''
#endif
>>> ([2,4..10] :: PrimVector Int) !! 3
8
>>> ([2,4..10] :: PrimVector Int) !! (-1)
10
>>> ("hello world" :: Bytes) !! 3
108
>>> ([2,4..10] :: PrimVector Int) !! 10
*** Exception: IndexOutOfVectorRange...
-}
(!!) :: (Vectors v a, HasCallStack) => v a -> Int -> a
(!!) = index'

{-|
#ifdef CN_DOC
修改下标的元素并返回新的数组切片，
支持负数下标（等价于 @len + i@），
如果下标超过范围则返回原切片。
#else
/O(n)/ Modify vector's element under given index,
negative index is supported(equivalent to @len + i@),
return original vector if index outside of range(-n ~ n-1).
#endif
>>> modifyAt ([2,4..10] :: PrimVector Int) 3 (+1)
[2,4,6,9,10]
>>> modifyAt ([2,4..10] :: PrimVector Int) (-1) (+1)
[2,4,6,8,11]
>>> modifyAt ([2,4..10] :: PrimVector Int) 10 (+1)
[2,4,6,8,10]
>>> modifyAt ("hello" :: Bytes) 3 Z.Data.ASCII.toUpper
"helLo"
-}
modifyAt :: (Vectors v a, HasCallStack) => v a -> Int -> (a -> a) -> v a
{-# INLINE modifyAt #-}
modifyAt v@(Slice arr s l) i f 
    | i < 0     = if i >= -l then Slice (modifyAtArray arr s l (l+i) f) 0 l else v
    | otherwise = if i < l   then Slice (modifyAtArray arr s l i f) 0 l else v

{-|
#ifdef CN_DOC
修改下标的元素并返回新的数组切片，
支持负数下标（等价于 @len + i@），
如果下标超过范围则报错 'IndexOutOfVectorRange'.
#else
/O(n)/ Modify vector's element under given index,
negative index is supported(equivalent to @len + i@),
throw 'IndexOutOfVectorRange' if index outside of range(-n ~ n-1).
#endif
>>> modifyAt' ([2,4..10] :: PrimVector Int) 3 (+1)
[2,4,6,9,10]
>>> modifyAt' ([2,4..10] :: PrimVector Int) (-1) (+1)
[2,4,6,8,11]
>>> modifyAt' ([2,4..10] :: PrimVector Int) 10 (+1)
*** Exception: IndexOutOfVectorRange...
>>> modifyAt' ("hello" :: Bytes) 3 Z.Data.ASCII.toUpper
"helLo"
-}
modifyAt' :: (Vectors v a, HasCallStack) => v a -> Int -> (a -> a) -> v a
{-# INLINE modifyAt' #-}
modifyAt' (Slice arr s l) i f
    | i < 0     = if i >= -l then Slice (modifyAtArray arr s l (l+i) f) 0 l else errorOutOfRange i
    | otherwise = if i < l   then Slice (modifyAtArray arr s l i f) 0 l else errorOutOfRange i

{-|
#ifdef CN_DOC
在指定下标前面插入一个元素，返回新的数组切片，
支持负数下标（等价于 @len + i@），
如果下标超过范围(-n ~ n)则返回原切片。
#else
/O(n)/ insert element to vector under given index,
negative index is supported(equivalent to @len + i@),
return original vector if index outside of range(-n ~ n).
#endif
-}
insertAt :: (Vectors v a, HasCallStack) => v a -> Int -> a -> v a
{-# INLINE insertAt #-}
insertAt v@(Slice arr s l) i x 
    | i < 0     = if i >= -l then Slice (insertAtArray arr s l (l+i) x) 0 (l+1) else v
    | otherwise = if i <= l   then Slice (insertAtArray arr s l i x) 0 (l+1) else v

{-|
#ifdef CN_DOC
在指定下标前面插入一个元素，返回新的数组切片，
支持负数下标（等价于 @len + i@），
如果下标超过范围(-n ~ n)则报错 'IndexOutOfVectorRange'。
#else
/O(n)/ insert element to vector under given index,
negative index is supported(equivalent to @len + i@),
throw 'IndexOutOfVectorRange' if index outside of range(-n ~ n).
#endif
-}
insertAt' :: (Vectors v a, HasCallStack) => v a -> Int -> a -> v a
{-# INLINE insertAt' #-}
insertAt' (Slice arr s l) i x
    | i < 0     = if i >= -l then Slice (insertAtArray arr s l (l+i) x) 0 (l+1) else errorOutOfRange i
    | otherwise = if i <= l   then Slice (insertAtArray arr s l i x) 0 (l+1) else errorOutOfRange i

{-|
#ifdef CN_DOC
删除下标的元素并返回新的数组切片，
支持负数下标（等价于 @len + i@），
如果下标超过范围(-n ~ n-1)则返回原切片。
#else
/O(n)/ Delete vector's element under given index.
negative index is supported(equivalent to @len + i@),
return original vector if index outside of range(-n ~ n-1).
#endif
-}
deleteAt :: (Vectors v a, HasCallStack) => v a -> Int -> v a
{-# INLINE deleteAt #-}
deleteAt v@(Slice arr s l) i
    | i < 0     = if i >= -l then Slice (deleteAtArray arr s l (l+i)) 0 (l-1) else v
    | otherwise = if i < l   then Slice (deleteAtArray arr s l i) 0 (l-1) else v

{-|
#ifdef CN_DOC
删除下标的元素并返回新的数组切片，
支持负数下标（等价于 @len + i@），
如果下标超过范围则报错 'IndexOutOfVectorRange'。
#else
/O(n)/ Delete vector's element under given index.
negative index is supported(equivalent to @len + i@),
throw 'IndexOutOfVectorRange' if index outside of range(-n ~ n-1).
#endif
-}
deleteAt' :: (Vectors v a, HasCallStack) => v a -> Int -> v a
{-# INLINE deleteAt' #-}
deleteAt' (Slice arr s l) i
    | i < 0     = if i >= -l then Slice (deleteAtArray arr s l (l+i)) 0 (l-1) else errorOutOfRange i
    | otherwise = if i < l   then Slice (deleteAtArray arr s l i) 0 (l-1) else errorOutOfRange i

#ifdef CN_DOC
-- | /O(n)/ 类似列表的 ':'，在数组头部新增一个元素。
#else
-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
#endif
cons :: Vectors v a => a -> v a -> v a
{-# INLINE cons #-}
cons x (Slice arr s l) = create_ (l+1) $ \ marr ->
    writeMutableArray marr 0 x >> copyArray marr 1 arr s l

#ifdef CN_DOC
-- | /O(n)/ 在数组最尾部新增一个元素。
#else
-- | /O(n)/ Append a byte to the end of a vector
#endif
snoc :: Vectors v a => v a -> a -> v a
{-# INLINE snoc #-}
snoc (Slice arr s l) x = create_ (l+1) $ \ marr ->
    copyArray marr 0 arr s l >> writeMutableArray marr l x

#ifdef CN_DOC
-- | /O(1)/ 提取切片的首元素和剩余部分，如果切片为空返回 'Nothing'。
#else
-- | /O(1)/ Extract the head and tail of a vector,
-- return 'Nothing' if input is empty.
#endif
uncons :: Vectors v a => v a -> Maybe (a, v a)
{-# INLINE uncons #-}
uncons (Slice arr s l)
    | l <= 0    = Nothing
    | otherwise = let !v = Slice arr (s+1) (l-1)
                  in case indexArray' arr s of (# x #) -> Just (x ,v)

#ifdef CN_DOC
-- | /O(1)/ 提取切片的起始部分和最后一个元素，如果切片为空返回 'Nothing'。
#else
-- | /O(1)/ Extract the head and tail of a vector,
-- return 'Nothing' if input is empty.
#endif
unsnoc :: Vectors v a => v a -> Maybe (v a, a)
{-# INLINE unsnoc #-}
unsnoc (Slice arr s l)
    | l <= 0    = Nothing
    | otherwise = let !v = Slice arr s (l-1)
                  in case indexArray' arr (s+l-1) of (# x #) -> Just (v, x)

{-|
#ifdef CN_DOC
/O(n)/ 提取切片所有可能的起始部分。
#else
/O(n)/ Return all initial segments of the given vector, empty first.
#endif
>>> inits ("hello" :: Bytes)
["","h","he","hel","hell","hello"]
-}
inits :: Vectors v a => v a -> [v a]
{-# INLINE inits #-}
inits (Slice arr s l) =  [Slice arr s n | n <- [0..l]]

{-|
#ifdef CN_DOC
/O(n)/ 提取切片所有可能的起始部分。
#else
/O(n)/ Return all final segments of the given vector, whole vector first.
#endif
>>> tails ("hello" :: Bytes)
["hello","ello","llo","lo","o",""]
-}

tails :: Vectors v a => v a -> [v a]
{-# INLINE tails #-}
tails (Slice arr s l) = [Slice arr (s+n) (l-n) | n <- [0..l]]

{-|
#ifdef CN_DOC
/O(n)/ 提取切片从 0 到 i （不包含）的元素，支持负数下标（末尾从 @-1@ 开始计算）。
#else
/O(1)/ Take first n elements, negative index is supported(equivalent to taking from 0 to last @-n@ elements).
This function is a total function, index exceeds range will be ingored, e.g.
#endif

@
   +----- take 5 ------+                        
   0    1    2    3    4    5    6    7    8    9   10
  "h    e    l    l    o         w    o    r    l    d"
 -11   -10  -9   -8   -7   -6   -5   -4   -3   -2   -1
   +-------------- take (-2) --------------+
@
>>> take 5 "hello world" :: Bytes
"hello"
>>> take (-2) "hello world" :: Bytes
"hello wor"
-}
take :: Vectors v a => Int -> v a -> v a
{-# INLINE take #-}
take n v@(Slice arr s l)
    | n < 0     = if n > -l then (Slice arr s (l+n)) else empty
    | otherwise = if n < l  then (Slice arr s n) else v
-- $property
-- props> \ (Positive n) -> take n (v :: Bytes) = pack $ List.take n (unpack v)
-- props> \ (Negative n) -> take n (v :: Bytes) = pack $ List.take (List.length + n) (unpack v)
-- props> take x (v :: Bytes) = slice 0 x v

{-|
#ifdef CN_DOC
/O(n)/ 丢掉切片从 0 到 i（不包含）的元素，支持负数下标（末尾从 @-1@ 开始计算）。
#else
/O(1)/ Drop first n elements, negative length is supported(equivalent to dropping from 0 to last @-n@ elements).
This function is a total function, index exceeds range will be ingored, e.g.
#endif
>>> drop 5 "hello world" :: Bytes
" world"
>>> drop (-3) "hello world" :: Bytes
"rld"
>>> drop 20 "hello world" :: Bytes
""
@
                            +-------- drop 5 --------+
   0    1    2    3    4    5    6    7    8    9   10
  "h    e    l    l    o         w    o    r    l    d"
 -11   -10  -9   -8   -7   -6   -5   -4   -3   -2   -1
                                           +drop (-3)+
@
-}
drop :: Vectors v a => Int -> v a -> v a
{-# INLINE drop #-}
drop n v@(Slice arr s l)
    | n < 0     = if n > -l then (Slice arr (s+l+n) (-n)) else v
    | otherwise = if n < l  then (Slice arr (s+n) (l-n)) else empty
-- $property
-- props> \ (Positive n) -> drop n (v :: Bytes) = pack $ List.drop n (unpack v)
-- props> \ (Negative n) -> drop n (v :: Bytes) = pack $ List.drop (List.length + n) (unpack v)
-- props> drop x (v :: Bytes) = slice x (length v) v

{-|
#ifdef CN_DOC
/O(1)/ 创建起始下标（包含）到结束下标（不包含）的新的切片，支持负数下标（含义和 'index', 'take' 一致），这是一个全函数，超出范围的下标会被忽略：
#else
/O(1)/ Extract a sub-range vector with give start index(included) and end index(not included).
This function is a total function just like 'take/drop', start and end index
exceeds range will be ingored, e.g.
#endif

@
        +-- slice 1 5 -+
   0    1    2    3    4    5    6    7    8    9   10
  "h    e    l    l    o         w    o    r    l    d"
 -11   -10  -9   -8   -7   -6   -5   -4   -3   -2   -1
        +------------ slice 1 (-1) -------------+
                           +slice (-6) (-2)+
@

>>> slice 1 5 "hello world" :: Bytes
"ello"
>>> slice 1 (-1) "hello world" :: Bytes
"ello worl"
>>> slice (-6) (-2) "hello world" :: Bytes
" wor"
>>> slice 2 20 "hello world" :: Bytes
"llo world"
-}
slice :: Vectors v a
      => Int          -- ^ slice start index
      -> Int          -- ^ slice end index
      -> v a -> v a
{-# INLINE slice #-}
slice x y (Slice arr s l) 
    | x' >= y' = empty
    | otherwise = Slice arr (s+x') (y'-x')
  where
    x' | x < 0 = if x > -l then x + l else 0
       | otherwise = if x < l then x else l
    y' | y < 0 = if y > -l then y + l else 0
       | otherwise = if y < l then y else l
-- $property:  slice x y === drop x . take y
-- prop> \ (Positive x) (Positive y) -> slice x y vs == (drop x . take y) (vs :: Bytes)
-- prop> \ (Positive x) (Positive y) -> slice x y vs == (drop x . take y) (vs :: PrimVector Int)
-- prop> \ (Positive x) (Positive y) -> slice x y vs == (drop x . take y) (vs :: Vector Integer)
-- prop> \ (Negative x) (Negative y) -> slice x y vs == (take y . drop x) (vs :: Bytes)
-- prop> \ (Negative x) (Negative y) -> slice x y vs == (take y . drop x) (vs :: PrimVector Int)
-- prop> \ (Negative x) (Negative y) -> slice x y vs == (take y . drop x) (vs :: Vector Int)

{-|
#ifdef CN_DOC
中缀版本'slice'。
#else
Infix version of 'slice'.
#endif
>>> "hello" !: (1, 3) :: Bytes
"el"
>>> "hello" !: (1, -1) :: Bytes
"ell"
>>> "hello" !: (-5, 2) :: Bytes
"he"
>>> "hello" !: (-2, 2) :: Bytes
""
>>> "hello" !: (2, 10) :: Bytes
"llo"
-}
(!:) :: Vectors v a
    => v a 
    -> (Int, Int)     -- ^ start and end index 
    -> v a
{-# INLINE (!:) #-}
v !: (s, e) = slice s e v

{-|
#ifdef CN_DOC
/O(1)/ 在指定下标处分割切片，@'splitAt' n x == ('take' n xs, 'drop' n xs)@
#else
/O(1)/ Split vector at given index. @'splitAt' n x == ('take' n xs, 'drop' n xs)@
#endif
>>> splitAt 3 ("hello" :: Bytes)
("hel","lo")
>>> splitAt 0 ("hello" :: Bytes)
("","hello")
>>> splitAt (-1) ("hello" :: Bytes)
("hell","o")
>>> splitAt 10 ("hello" :: Bytes)
("hello","")
-}
splitAt :: Vectors v a => Int -> v a -> (v a, v a)
{-# INLINE splitAt #-}
splitAt z (Slice arr s l) 
    | z < 0     = 
        let !z' = if z > -l then z + l else 0 
        in (Slice arr s z', Slice arr (s+z') (l-z'))
    | otherwise = 
        let !z' = if z < l then z else l
        in (Slice arr s z', Slice arr (s+z') (l-z'))
-- $property: splitAt n xs == (take n xs, drop n xs)
-- prop> splitAt n (xs :: Bytes) == (take n xs, drop n xs)
-- prop> splitAt n (xs :: PrimVector Int) == (take n xs, drop n xs)
-- prop> splitAt n (xs :: Vector Integer) == (take n xs, drop n xs)

{-|
#ifdef CN_DOC
中缀版本'splitAt'。
#else
Infix version of 'splitAt'.
#endif
>>> ("hello" :: Bytes) !/ 3
("hel","lo")
>>> ("hello" :: Bytes) !/ 0
("","hello")
>>> ("hello" :: Bytes) !/ (-1)
("hell","o")
>>> ("hello" :: Bytes) !/ 10
("hello","")
-}
(!/) :: Vectors v a => v a -> Int -> (v a, v a)
{-# INLINE (!/) #-}
v !/ x = splitAt x v

{-|
#ifdef CN_DOC
/O(n)/ 从切片开始取元素直到判断函数不满足为止。
#else
/O(n)/ Applied to a predicate @p@ and a vector @vs@,
returns the longest prefix (possibly empty) of @vs@ of elements that
satisfy @p@.
#endif
>>> takeWhile Z.Data.ASCII.isDigit "12345abc" :: Bytes
"12345"
-}
takeWhile :: Vectors v a => (a -> Bool) -> v a -> v a
{-# INLINE [1] takeWhile #-}
{-# RULES "takeWhile/breakEq1" forall w. takeWhile (w `neWord8`) = fst . break (w `eqWord8`) #-}
{-# RULES "takeWhile/breakEq2" forall w. takeWhile (`neWord8` w) = fst . break (`eqWord8` w) #-}
takeWhile f v@(Slice arr s _) =
    case findElem (not . f) v of
        Just (IPair i _) -> Slice arr s i
        _                -> v
-- $property
-- prop> \ (Fun _ f) -> takeWhile f (xs :: Vector Integer) == pack (List.takeWhile f (unpack xs))
-- prop> \ (Fun _ f) -> takeWhile f (xs :: PrimVector Int) == pack (List.takeWhile f (unpack xs))
-- prop> \ (Fun _ f) -> takeWhile f (xs :: Bytes) == pack (List.takeWhile f (unpack xs))

{-|
#ifdef CN_DOC
/O(n)/ 从切片尾部取元素直到判断函数不满足为止。
#else
/O(n)/ Applied to a predicate @p@ and a vector @vs@,
returns the longest suffix (possibly empty) of @vs@ of elements that
satisfy @p@.
#endif
>>> takeWhileR Z.Data.ASCII.isLetter "12345abc" :: Bytes
"abc"
-}
takeWhileR :: Vectors v a => (a -> Bool) -> v a -> v a
{-# INLINE [1] takeWhileR #-}
{-# RULES "takeWhileR/breakREq1" forall w. takeWhileR (w `neWord8`) = snd . breakR (w `eqWord8`) #-}
{-# RULES "takeWhileR/breakREq2" forall w. takeWhileR (`neWord8` w) = snd . breakR (`eqWord8` w) #-}
takeWhileR f v@(Slice arr s l) =
    case findElemR (not . f) v of
        Just (IPair i _) -> Slice arr (s+l+i+1) (-i-1)
        _                -> v
-- $property
-- prop> \ (Fun _ f) -> takeWhileR f (xs :: Vector Integer) == reverse (takeWhile f (reverse xs))
-- prop> \ (Fun _ f) -> takeWhileR f (xs :: PrimVector Int) == reverse (takeWhile f (reverse xs))
-- prop> \ (Fun _ f) -> takeWhileR f (xs :: Bytes) == reverse (takeWhile f (reverse xs))

{-|
#ifdef CN_DOC
/O(n)/ 从切片开始删除元素直到判断函数不满足为止。
#else
/O(n)/ Applied to a predicate @p@ and a vector @vs@,
returns the suffix (possibly empty) remaining after 'takeWhile' @p vs@.
#endif
>>> dropWhile Z.Data.ASCII.isDigit "12345abc" :: Bytes
"abc"
-}
dropWhile :: Vectors v a => (a -> Bool) -> v a -> v a
{-# INLINE [1] dropWhile #-}
{-# RULES "dropWhile/breakEq1" forall w. dropWhile (w `neWord8`) = snd . break (w `eqWord8`) #-}
{-# RULES "dropWhile/breakEq2" forall w. dropWhile (`neWord8` w) = snd . break (`eqWord8` w) #-}
dropWhile f v@(Slice arr s l) =
    case findElem (not . f) v of
        Just (IPair i _) -> Slice arr (s+i) (l-i)
        _                -> empty

{-|
#ifdef CN_DOC
/O(n)/ 从切片尾部删除元素直到判断函数不满足为止。
#else
/O(n)/ Applied to a predicate @p@ and a vector @vs@,
returns the prefix (possibly empty) remaining before 'takeWhileR' @p vs@.
#endif
>>> dropWhileR Z.Data.ASCII.isLetter "12345abc" :: Bytes
"12345"
-}
dropWhileR :: Vectors v a => (a -> Bool) -> v a -> v a
{-# INLINE [1] dropWhileR #-}
{-# RULES "dropWhileR/breakEq1" forall w. dropWhileR (w `neWord8`) = fst . breakR (w `eqWord8`) #-}
{-# RULES "dropWhileR/breakEq2" forall w. dropWhileR (`neWord8` w) = fst . breakR (`eqWord8` w) #-}
dropWhileR f v@(Slice arr s l) =
    case findElemR (not . f) v of
        Just (IPair i _) -> Slice arr s (s+l+i+1)
        _                -> empty

{-|
#ifdef CN_DOC
/O(n)/ 从切片两侧删除元素直到判断函数不满足为止。
#else
/O(n)/ @dropAround f = dropWhile f . dropWhileR f@
#endif
>>> dropAround Z.Data.ASCII.isSpace " hello " :: Bytes
"hello"
-}
dropAround :: Vectors v a => (a -> Bool) -> v a -> v a
{-# INLINE dropAround #-}
dropAround f = dropWhile f . dropWhileR f

{-|
#ifdef CN_DOC
从第一个满足判断函数的元素位置分割切片。
#else
/O(n)/ Split the vector into the longest prefix of elements that do not satisfy the predicate and the rest without copying, @break (==x)@ will be rewritten using a @memchr@ if possible.
#endif
>>> break (== 10) ([2,4..20] :: PrimVector Int)
([2,4,6,8],[10,12,14,16,18,20])
-}
break :: Vectors v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE break #-}
break f vs@(Slice arr s l) =
    case findElem f vs of
        Just (IPair n _) -> 
            let !v1 = Slice arr s n
                !v2 = Slice arr (s+n) (l-n)
            in (v1, v2)
        _ -> (vs, empty)

{-|
#ifdef CN_DOC
从第一个不满足判断函数的元素位置分割切片。
#else
/O(n)/ Split the vector into the longest prefix of elements that satisfy the predicate and the rest without copying.
@span (/=x)@ will be rewritten using a @memchr@ if possible.
#endif
>>> span (<= 10) ([2,4..20] :: PrimVector Int)
([2,4,6,8,10],[12,14,16,18,20])
-}
span :: Vectors v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE [1] span #-}
{-# RULES "spanNEq/breakEq1" forall w. span (w `neWord8`) = break (w `eqWord8`) #-}
{-# RULES "spanNEq/breakEq2" forall w. span (`neWord8` w) = break (`eqWord8` w) #-}
span f = break (not . f)

{-|
#ifdef CN_DOC
从倒数第一个满足判断函数的元素位置分割切片。
#else
'breakR' behaves like 'break' but apply predictor from the end of the vector.
#endif
>>> breakR (== 10) ([2,4..20] :: PrimVector Int)
([2,4,6,8,10],[12,14,16,18,20])
-}
breakR :: Vectors v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE breakR #-}
breakR f vs@(Slice arr s l) =
    case findElemR f vs of
        Just (IPair n _) -> 
            let !v1 = Slice arr s (l+n+1)
                !v2 = Slice arr (s+l+n+1) (-n-1)
            in (v1, v2)
        _ -> (empty, vs)

{-|
#ifdef CN_DOC
从倒数第一个不满足判断函数的元素位置分割切片。
#else
'spanR' behaves like 'span' but apply predictor from the end of the vector.
#endif
-}
spanR :: Vectors v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE [1]  spanR #-}
{-# RULES "spanNEq/breakREq1" forall w. spanR (w `neWord8`) = breakR (w `eqWord8`) #-}
{-# RULES "spanNEq/breakREq2" forall w. spanR (`neWord8` w) = breakR (`eqWord8` w) #-}
spanR f = breakR (not . f)

{-|
#ifdef CN_DOC
在第一个匹配切片的元素位置分割切片。
#else
Break a vector on a subvector, returning a pair of the part of the
vector prior to the match, and the rest of the vector, e.g.
#endif
>>> breakOn "wor" ("hello, world" :: Bytes)
("hello, ","world")
>>> breakOn "xx" ("hello, world" :: Bytes)
("hello, world","")
-}
breakOn :: (Vectors v a, Eq a) => v a -> v a -> (v a, v a)
{-# INLINABLE breakOn #-}
breakOn needle haystack@(Slice arr s l) =
    case indices False needle haystack of
        (i:_) -> let !v1 = Slice arr s i
                     !v2 = Slice arr (s+i) (l-i)
                 in (v1, v2)
        _     -> (haystack, empty)

{-|
#ifdef CN_DOC
把相邻相同的元素放到新的切片里。
#else
/O(n)/ Group elements by equality.
#endif
>>> group ("aabbccccd" :: Bytes)
["aa","bb","cccc","d"]
-}
group :: (Vectors v a, Eq a) => v a -> [v a]
{-# INLINABLE group #-}
group = groupBy (==)

{-|
#ifdef CN_DOC
把满足条件的相邻元素放到新的切片里，注意：条件测试只会和每个子串的首元素进行。
#else
/O(n)/ Group elements by a predicate.
#endif
>>> groupBy (\ x y -> abs(x-y) < 10) ([1,2,3,10,20,30,35,40] :: PrimVector Int)
[[1,2,3,10],[20],[30,35],[40]]
-}
groupBy :: forall v a. Vectors v a =>  (a -> a -> Bool) -> v a -> [v a]
{-# INLINABLE groupBy #-}
groupBy f (Slice arr s0 l0) = go s0 l0
  where
    go !s !l
        | l == 0 = []
        | otherwise = 
            case indexArray' arr s of
                (# x #) -> 
                    case findElem @v (not . f x) (Slice arr (s+1) (l-1)) of
                        Just (IPair n _) -> Slice arr s (n+1) : go (s+n+1) (l-n-1)
                        _ -> [Slice arr s l]
-- $property
-- prop> \ (Fun _ (curry -> f)) -> groupBy f (xs :: Vector Integer) == List.map pack (List.groupBy f (unpack xs))

{-|
#ifdef CN_DOC
/O(n)/ 删除切片的前缀，如果提供的不是切片的前缀，返回原切片。
#else
/O(n)/ The 'stripPrefix' function takes two vectors and returns
the remainder of the second iff the first is its prefix, 
and the original vector otherwise.
#endif
>>> stripPrefix [0,1,2] [0..10] :: PrimVector Int
[3,4,5,6,7,8,9,10]
>>> stripPrefix [1,2,3] [0..10] :: PrimVector Int
[0,1,2,3,4,5,6,7,8,9,10]
-}
stripPrefix :: (Vectors v a, Eq (v a))
            => v a  -- ^ the prefix to be tested
            -> v a  -- ^ string to be modified
            -> v a
{-# INLINABLE stripPrefix #-}
stripPrefix v1@(Slice _ _ l1) v2@(Slice arr s l2)
   | v1 `isPrefixOf` v2 = Slice arr (s+l1) (l2-l1)
   | otherwise = v2

{-|
#ifdef CN_DOC
/O(m)/ 判断是不是切片的前缀。
#else
/O(m)/ The 'isPrefix' function returns 'True' if the first argument is a prefix of the second.
#endif
-}
isPrefixOf :: forall v a. (Vectors v a, Eq (v a))
           => v a       -- ^ the prefix to be tested
           -> v a -> Bool
{-# INLINABLE isPrefixOf #-}
isPrefixOf (Slice arrA sA lA) (Slice arrB sB lB)
    | lA == 0 = True
    | lA > lB = False
    | otherwise = (==) @(v a) (Slice arrA sA lA) (Slice arrB sB lA)
-- $property
-- prop> \ (Positive n) -> isPrefixOf (take n vs) (vs :: Vector Integer) == True
-- prop> \ (Positive n) -> isPrefixOf (take n vs) (vs :: PrimVector Int) == True
-- prop> \ (Positive n) -> isPrefixOf (take n vs) (vs :: Bytes) == True

{-|
#ifdef CN_DOC
/O(n)/ 寻找两个切片的共同前缀，返回前缀和两个切片的剩余部分。
#else
/O(n)/ Find the longest non-empty common prefix of two strings
and return it, along with the suffixes of each string at which they
no longer match. e.g.
#endif
>>> commonPrefix "foobar" ("fooquux" :: Bytes)
("foo","bar","quux")
>>> commonPrefix "veeble" ("fetzer" :: Bytes)
("","veeble","fetzer")
-}
commonPrefix :: (Vectors v a, Eq a) => v a -> v a -> (v a, v a, v a)
{-# INLINABLE commonPrefix #-}
commonPrefix vA@(Slice arrA sA lA) vB@(Slice arrB sB lB) = go sA sB
  where
    !endA = sA + lA
    !endB = sB + lB
    go !i !j | i >= endA = let !vB' = Slice arrB (sB+i-sA) (lB-i+sA) in (vA, empty, vB')
             | j >= endB = let !vA' = Slice arrA (sA+j-sB) (lA-j+sB) in (vB, vA', empty)
             | indexArray arrA i == indexArray arrB j = go (i+1) (j+1)
             | otherwise =
                let vB' = Slice arrB (sB+i-sA) (lB-i+sA)
                    vA' = Slice arrA (sA+j-sB) (lA-j+sB)
                    vC  = Slice arrA sA (i-sA)
                in (vC, vA', vB')
-- $property
-- prop> case commonPrefix va vb of (vp, va', vb') -> vp <> va' == va && vp <> vb' == (vb :: Vector Integer)
-- prop> case commonPrefix va vb of (vp, va', vb') -> vp <> va' == va && vp <> vb' == (vb :: PrimVector Int)
-- prop> case commonPrefix va vb of (vp, va', vb') -> vp <> va' == va && vp <> vb' == (vb :: Bytes)


{-|
#ifdef CN_DOC 
/O(n)/ 删除切片的后缀，如果提供的不是切片的后缀，返回原切片。
#else
/O(n)/ The 'stripSuffix' function takes two vectors and returns
the remainder of the second iff the first is its suffix, and the original vector otherwise.
#endif
>>> stripSuffix [8,9,10] [0..10] :: PrimVector Int
[0,1,2,3,4,5,6,7]
>>> stripSuffix [1,2,3] [0..10] :: PrimVector Int
[0,1,2,3,4,5,6,7,8,9,10]
-}
stripSuffix :: (Vectors v a, Eq (v a)) => v a -> v a -> v a
{-# INLINABLE stripSuffix #-}
stripSuffix v1@(Slice _ _ l1) v2@(Slice arr s l2)
   | v1 `isSuffixOf` v2 = Slice arr s (l2-l1)
   | otherwise = v2

{-|
#ifdef CN_DOC
/O(m)/ 判断是不是切片的后缀。
#else
/O(m)/ The 'isSuffixOf' function takes two vectors and returns 'True'
if the first is a suffix of the second.
#endif
-}
isSuffixOf :: forall v a. (Vectors v a, Eq (v a)) => v a -> v a -> Bool
{-# INLINABLE isSuffixOf #-}
isSuffixOf (Slice arrA sA lA) (Slice arrB sB lB)
    | lA == 0 = True
    | lA > lB = False
    | otherwise = (==) @(v a) (Slice arrA sA lA) (Slice arrB (sB+lB-lA) lA)

{-|
#ifdef CN_DOC
/O(n+m)/ 判断是不是切片的中缀，即子串是否在切片中出现过。
#else
/O(n+m)/ Check whether one vector is a subvector of another.
#endif
-}
isInfixOf :: (Vectors v a, Eq a) => v a -> v a -> Bool
{-# INLINABLE isInfixOf #-}
isInfixOf needle haystack = null haystack || not (List.null (indices False needle haystack))
-- $property
-- prop> (needle :: Vector Integer) `isInfixOf` haystack == (null haystack || indices False needle haystack /= [])
-- prop> (needle :: PrimVector Int) `isInfixOf` haystack == (null haystack || indices False needle haystack /= [])
-- prop> (needle :: Bytes) `isInfixOf` haystack == (null haystack || indices False needle haystack /= [])


{-|
#ifdef CN_DOC
/O(n)/ 使用元素来分割切片。
#else
/O(n)/ Break a vector into pieces separated by the delimiter element
consuming the delimiter. NOTE, this function behavior different with bytestring's. see
<https://github.com/haskell/bytestring/issues/56 #56>.
#endif
>>> split (c2w '\n') ("a\nb\nd\ne" :: Bytes)
["a","b","d","e"]
>>> split (c2w 'a')  ("aXaXaXa" :: Bytes)
["","X","X","X",""]
>>> split (c2w 'x') ("x" :: Bytes)          
["",""]
-}
split :: (Vectors v a, Eq a) => a -> v a -> [v a]
{-# INLINABLE split #-}
split x = splitWith (==x)
-- $property
-- prop>\ (xs :: Vector Integer) -> intercalate (singleton c) (split c xs) == xs
-- prop>\ (xs :: Vector Integer) -> split x xs == splitWith (==x) xs
-- prop>\ (xs :: PrimVector Int) -> intercalate (singleton c) (split c xs) == xs
-- prop>\ (xs :: PrimVector Int) -> split x xs == splitWith (==x) xs
-- prop>\ (xs :: Bytes) -> intercalate (singleton c) (split c xs) == xs
-- prop>\ (xs :: Bytes) -> split x xs == splitWith (==x) xs

{-|
#ifdef CN_DOC
用子串来分割切片，注意一个空的子串会把每一个元素都切割开。
#else
/O(m+n)/ Break haystack into pieces separated by needle.
Note: An empty needle will essentially split haystack element
by element.
#endif
>>> splitOn "\r\n" ("a\r\nb\r\nd\r\ne" :: Bytes)
["a","b","d","e"]
>>> splitOn "aaa"  ("aaaXaaaXaaaXaaa" :: Bytes)
["","X","X","X",""]
>>> splitOn "x"  ("x" :: Bytes)
["",""]
-}
splitOn :: (Vectors v a, Eq a) => v a -> v a -> [v a]
{-# INLINABLE splitOn #-}
splitOn needle haystack@(Slice arr s0 l0) =
    go s0 (indices False needle haystack)
  where
    !nl = length needle
    go !s (i:is) = let !v = Slice arr s (s0+i-s) in v : go (s0+i+nl) is
    go !s _      = let !v = Slice arr s (s0+l0-s) in [v]
-- $property
-- prop>\ (xs :: Vector Integer) -> intercalate s (splitOn s xs) == xs
-- prop>\ (xs :: Vector Integer) -> splitOn (singleton c) xs     == split c xs
-- prop>\ (xs :: PrimVector Int) -> intercalate s (splitOn s xs) == xs
-- prop>\ (xs :: PrimVector Int) -> splitOn (singleton c) xs     == split c xs
-- prop>\ (xs :: Bytes) -> intercalate s (splitOn s xs) == xs
-- prop>\ (xs :: Bytes) -> splitOn (singleton c) xs     == split c xs

{-|
#ifdef CN_DOC
/O(m+n)/ 替换切片中所有不重叠的@needle@为@replacement@。
#else
/O(m+n)/ Replace every non-overlapping occurrence of @needle@ in
@haystack@ with @replacement@.
#endif
-}
replace :: forall v a. (Vectors v a, Eq a)
        => v a      -- ^ needle to search for. If this string is empty, an error will occur.
        -> v a      -- ^ replacement to replace needle with.
        -> v a      -- ^ haystack in which to search.
        -> v a
{-# INLINABLE replace #-}
replace needle@(Slice _ _ nl) (Slice rarr rs rl) haystack@(Slice harr hs hl) =
    create_ result_len $ \ marr -> loop marr ixs 0 0
  where
    ixs = indices False needle haystack
    result_len = hl - (nl - rl) * List.length ixs

    loop :: forall s. Mutable (BaseArray v) s a -> [Int] -> Int -> Int -> ST s ()
    loop marr (i:is) !o !d = do
        let d0 = d + i - o
            d1 = d0 + rl
        copyArray marr d  harr (hs+o) (i-o)
        copyArray marr d0 rarr rs rl 
        loop marr is (i + nl) d1
    loop marr _      o d = copyArray marr d harr (hs+o) (result_len - d)
{-| $property
prop> replace needle replacement (haystack :: PrimVector Int) == intercalate replacement (splitOn needle haystack)
prop> replace needle replacement (haystack :: Vector Integer) == intercalate replacement (splitOn needle haystack)
prop> replace needle replacement (haystack :: Bytes) == intercalate replacement (splitOn needle haystack)
-}


{-|
#ifdef CN_DOC
/O(n)/ 通过匹配元素来分割切片。
#else
/O(n)/ Splits a vector into components delimited by
separators, where the predicate returns True for a separator element.
The resulting components do not contain the separators.  Two adjacent
separators result in an empty component in the output.  eg.
#endif
>>> splitWith (== c2w 'a') ("aabbaca" :: Bytes)
["","","bb","c",""]
>>> splitWith (== c2w 'a') ("" :: Bytes)
[""]
-}
splitWith :: Vectors v a => (a -> Bool) -> v a -> [v a]
{-# INLINABLE splitWith #-}
splitWith f = go
  where
    go v@(Slice arr s l)
        | l == 0    = [empty]
        | otherwise =
            case findElem f v of
                Just (IPair n _) -> Slice arr s n : go (Slice arr (s+n+1) (l-n-1))
                _                -> [v]

#ifdef CN_DOC
-- | /O(n)/ 使用 ASCII 空格（空格、制表符）分割字符串。
#else
-- | /O(n)/ Breaks a 'Bytes' up into a list of words, delimited by ascii space(s).
#endif
words ::  Bytes -> [Bytes]
{-# INLINE words #-}
words (Slice arr s l) = go s s
  where
    !end = s + l
    go :: Int -> Int -> [Bytes]
    go !s' !i | i >= end =
                    if s' == end
                    then []
                    else let !v = Slice arr s' (end-s') in [v]
              | isSpace (indexArray arr i) =
                    if s' == i
                    then go (i+1) (i+1)
                    else
                    let !v = Slice arr s' (i-s') in v : go (i+1) (i+1)
              | otherwise = go s' (i+1)

#ifdef CN_DOC
-- | /O(n)/ 使用换行符 @\n@ 分割字符串，注意该函数不考虑 @\r@ 的情况。
#else
-- | /O(n)/ Breaks a 'Bytes' up into a list of lines, delimited by ascii @\n@,
-- The resulting strings do not contain newlines.
-- Note that it __does not__ regard CR (@\r@) as a newline character.
#endif
lines ::  Bytes -> [Bytes]
{-# INLINABLE lines #-}
lines = splitWith (== 10)

#ifdef CN_DOC
-- | /O(n)/ 使用 ASCII 空格连接字符串。
#else
-- | /O(n)/ Joins words with ascii space.
#endif
unwords :: [Bytes] -> Bytes
{-# INLINABLE unwords #-}
unwords = intercalateElem 32

#ifdef CN_DOC
-- | /O(n)/ 使用 @\n@ 连接字符串。该函数和 'Prelude.unlines' 不同的是不会在结果的最后添加 @\n@。
#else
-- | /O(n)/ Joins lines with ascii @\n@.
-- NOTE: This functions is different from 'Prelude.unlines', it DOES NOT add a trailing @\n@.
#endif
unlines :: [Bytes] -> Bytes
{-# INLINABLE unlines #-}
unlines = intercalateElem 10

#ifdef CN_DOC
-- | 向开头添加填补元素把切片填补到指定长度，如果本身长度已经足够则不做改变。
#else
-- | Add padding to the left so that the whole vector's length is at least n.
#endif
padLeft :: Vectors v a => Int -> a -> v a -> v a
{-# INLINABLE padLeft #-}
padLeft n x v@(Slice arr s l) | n <= l = v
                            | otherwise = create_ n (\ marr -> do
                                    setMutableArray marr 0 (n-l) x
                                    copyArray marr (n-l) arr s l)

#ifdef CN_DOC
-- | 向末尾添加填补元素把切片填补到指定长度，如果本身长度已经足够则不做改变。
#else
-- | Add padding to the right so that the whole vector's length is at least n.
#endif
padRight :: Vectors v a => Int -> a -> v a -> v a
{-# INLINABLE padRight #-}
padRight n x v@(Slice arr s l) | n <= l = v
                             | otherwise = create_ n (\ marr -> do
                                    copyArray marr 0 arr s l
                                    setMutableArray marr l (n-l) x)

--------------------------------------------------------------------------------
-- Transform

{-|
#ifdef CN_DOC
/O(n)/ 颠倒切片元素的顺序生成新切片。
#else
/O(n)/ 'reverse' @vs@ efficiently returns the elements of @xs@ in reverse order.
#endif
>>> reverse [1..10] :: Vector Integer
[10,9,8,7,6,5,4,3,2,1]
>>> reverse "hello world" :: Bytes
"dlrow olleh"
-}
reverse :: forall v a. (Vectors v a) => v a -> v a
{-# INLINABLE reverse #-}
reverse (Slice arr s l) = create_ l (go s (l-1))
  where
    go :: Int -> Int -> Mutable (BaseArray v) s a -> ST s ()
    go !i !j !marr | j < 0 = return ()
                   | j >= 3 = do  -- a bit of loop unrolling here
                        indexArrayM arr i >>= writeMutableArray marr j
                        indexArrayM arr (i+1) >>= writeMutableArray marr (j-1)
                        indexArrayM arr (i+2) >>= writeMutableArray marr (j-2)
                        indexArrayM arr (i+3) >>= writeMutableArray marr (j-3)
                        go (i+4) (j-4) marr
                   | otherwise = do
                        indexArrayM arr i >>= writeMutableArray marr j
                        go (i+1) (j-1) marr
-- $property
-- prop> reverse (xs :: Vector Integer) == pack (List.reverse (unpack xs))


{-|
#ifdef CN_DOC
/O(n)/ 把元素混进切片的每个元素之间。
#else
/O(n)/ The 'intersperse' function takes an element and a
vector and \`intersperses\' that element between the elements of
the vector.  It is analogous to the intersperse function on
Lists.
#endif
>>> intersperse (-1) [0..10] :: PrimVector Int
[0,-1,1,-1,2,-1,3,-1,4,-1,5,-1,6,-1,7,-1,8,-1,9,-1,10]
-}
intersperse :: forall v a. Vectors v a => a -> v a -> v a
{-# INLINE [1] intersperse #-}
{-# RULES "intersperse/Bytes" intersperse = intersperseBytes #-}
intersperse x v@(Slice arr s l) | l <= 1 = v
                              | otherwise = create_ (2*l-1) (go s 0)
   where
    !end = s+l-1
    go :: Int         -- the reading index of orginal bytes
       -> Int         -- the writing index of new buf
       -> Mutable (BaseArray v) s a -- the new buf
       -> ST s ()
    go !i !j !marr
        | i == end = writeMutableArray marr j =<< indexArrayM arr i
        | i <= end - 4 = do -- a bit of loop unrolling
            writeMutableArray marr j =<< indexArrayM arr i
            writeMutableArray marr (j+1) x
            writeMutableArray marr (j+2) =<< indexArrayM arr (i+1)
            writeMutableArray marr (j+3) x
            writeMutableArray marr (j+4) =<< indexArrayM arr (i+2)
            writeMutableArray marr (j+5) x
            writeMutableArray marr (j+6) =<< indexArrayM arr (i+3)
            writeMutableArray marr (j+7) x
            go (i+4) (j+8) marr
        | otherwise = do
            writeMutableArray marr j =<< indexArrayM arr i
            writeMutableArray marr (j+1) x
            go (i+1) (j+2) marr

-- | /O(n)/ Special 'intersperse' for 'Bytes' using SIMD
intersperseBytes :: Word8 -> Bytes -> Bytes
{-# INLINE intersperseBytes #-}
intersperseBytes c v@(PrimVector (PrimArray ba#) offset l)
    | l <= 1 = v
    | otherwise = unsafeDupablePerformIO $ do
        mba@(MutablePrimArray mba#) <- newMutableArray n
        z_intersperse mba# ba# offset l c
        ba <- unsafeFreezeMutableArray mba
        return $! Slice ba 0 n
  where n = 2*l-1

{-|
#ifdef CN_DOC
/O(n)/ 把切片混进切片列表的每个切片之间。
#else
/O(n)/ The 'intercalate' function takes a vector and a list of
vectors and concatenates the list after interspersing the first
argument between each element of the list.
Note: 'intercalate' will force the entire vector list.
#endif
>>> intercalate "," ["hello", "bar", "qux"] :: Bytes
"hello,bar,qux"
-}
intercalate :: Vectors v a => v a -> [v a] -> v a
{-# INLINABLE intercalate #-}
intercalate s = concat . List.intersperse s

{-|
#ifdef CN_DOC
/O(n)/ 把元素混进切片列表的每个切片之间。
#else
/O(n)/ An efficient way to join vector with an element.
#endif
>>> intercalateElem (c2w ',') ["hello", "bar", "qux"] :: Bytes
"hello,bar,qux"
-}
intercalateElem :: forall v a. Vectors v a => a -> [v a] -> v a
{-# INLINABLE intercalateElem #-}
intercalateElem _ [] = empty
intercalateElem _ [v] = v
intercalateElem w vs = create_ (len vs 0) (go 0 vs)
  where
    len []             !acc = acc
    len [Slice _ _ l]    !acc = l + acc
    len (Slice _ _ l:vs') !acc = len vs' (acc+l+1)
    go :: forall s. Int -> [v a] -> Mutable (BaseArray v) s a -> ST s ()
    go !_ []               !_    = return ()
    go !i (Slice arr s l:[]) !marr = copyArray marr i arr s l
    go !i (Slice arr s l:vs') !marr = do
        let !i' = i + l
        copyArray marr i arr s l
        writeMutableArray marr i' w
        go (i'+1) vs' marr
-- $property
-- prop> intercalateElem c v == (intercalate (singleton c) v :: Vector Integer)
-- prop> intercalateElem c v == (intercalate (singleton c) v :: PrimVector Int)
-- prop> intercalateElem c v == (intercalate (singleton c) v :: Bytes)

{-| 
#ifdef CN_DOC
对切片列表元素进行转置。
#else
The 'transpose' function transposes the rows and columns of its vector argument.
#endif
>>> transpose ["foo", "bar", "qux"] :: [Bytes]
["fbq","oau","orx"]
-}
transpose :: Vectors v a => [v a] -> [v a]
{-# INLINABLE transpose #-}
transpose vs =
    List.map (packN n) . List.transpose . List.map unpack $ vs
  where n = List.length vs

--------------------------------------------------------------------------------
--  Zipping

{-|
#ifdef CN_DOC
类似 @'List.zipWith'@，把两个切片的元素一对一合成新的元素，返回得切片长度
等于较短的那个输入，注意新的切片里每一个元素都会被求值到弱常态。
#else
'zipWith' zip two vector with a zipping function.
For example, @'zipWith' (+)@ is applied to two vector to produce
a vector of corresponding sums, the result will be evaluated strictly.
#endif
>>> zipWith replicate ([1,2..5] :: PrimVector Int) ("abcdefg" :: Bytes) :: Vector Bytes
["a","bb","ccc","dddd","eeeee"]
-}
zipWith :: forall v a u b w c. (Vectors v a, Vectors u b, Vectors w c)
         => (a -> b -> c) -> v a -> u b -> w c
{-# INLINABLE zipWith #-}
zipWith f (Slice arrA sA lA) (Slice arrB sB lB) = create_ len (go 0)
  where
    !len = min lA lB
    go :: forall s. Int -> Mutable (BaseArray w) s c -> ST s ()
    go !i !marr
        | i >= len = return ()
        | otherwise =
            case indexArray' arrA (i+sA) of
                 (# a #) -> case indexArray' arrB (i+sB) of
                     (# b #) -> do
                        let !c = f a b 
                        writeMutableArray marr i c
                        go (i+1) marr


{-|
#ifdef CN_DOC
和 'zipWith' 相反，'unzipWith' 把切片的每一个元素分解成两个，返回两个新的切片。
#else
'unzipWith' disassemble a vector with a disassembling function,
#endif
>>> let bv = (["a","bb","ccc","dddd","eeeee"] :: Vector Bytes)
>>> unzipWith (\ v -> (length v, padLeft 10 (c2w 'x') v)) bv :: (PrimVector Int, Vector Bytes)
([1,2,3,4,5],["xxxxxxxxxa","xxxxxxxxbb","xxxxxxxccc","xxxxxxdddd","xxxxxeeeee"])
-}
unzipWith :: forall v a u b w c. (Vectors v a, Vectors u b, Vectors w c)
          => (a -> (b, c)) -> v a -> (u b, w c)
{-# INLINABLE unzipWith #-}
unzipWith f (Slice arr s l) = runST $ do
    !marrB <- newMutableArray l
    !marrC <- newMutableArray l
    let go !i =
            if i >= l 
            then do
                !ba0 <- unsafeFreezeMutableArray marrB
                !ba1 <- unsafeFreezeMutableArray marrC
                return (Slice ba0 0 l, Slice ba1 0 l)
            else case indexArray' arr (i+s) of
                (# a #) -> case f a of 
                    (!b, !c) -> do
                        writeMutableArray marrB i b
                        writeMutableArray marrC i c
                        go (i+1)
    go 0

--------------------------------------------------------------------------------
-- Scans

{-|
#ifdef CN_DOC
'scanl' 和 'foldl' 很像，但是会把每一步得到得状态值记录下来保存在切片里并返回。
#else
'scanl' is similar to 'foldl', but returns a list of successive
reduced values from the left: @scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]@.
#endif
-}
scanl :: forall v u a b. (Vectors v a, Vectors u b) => (b -> a -> b) -> b -> v a -> u b
{-# INLINE scanl #-}
scanl f z (Slice arr s l) =
    create_ (l+1) (\ marr -> writeMutableArray marr 0 z >> go z s 1 marr)
  where
    go :: b  -> Int -> Int -> Mutable (BaseArray u) s b -> ST s ()
    go !acc !i !j !marr
        | j > l = return ()
        | otherwise = do
            x <- indexArrayM arr i
            let !acc' = acc `f` x
            writeMutableArray marr j acc'
            go acc' (i+1) (j+1) marr
-- $property
-- prop> \ (Fun _ (curry -> f)) -> (scanl f z xs :: Vector Integer) !! (-1) == foldl' f z (xs :: Vector Integer)
-- prop> \ (Fun _ (curry -> f)) -> (scanl f z xs :: PrimVector Int) !! (-1) == foldl' f z (xs :: PrimVector Int)
-- prop> \ (Fun _ (curry -> f)) -> (scanl f z xs :: Bytes) !! (-1) == foldl' f z (xs :: Bytes)

{-|
#ifdef CN_DOC
'scanl' 的一个变种，使用第一个元素来作为起始状态。
#else
'scanl1' is a variant of 'scanl that has no starting value argument.
#endif
-}
scanl1 :: forall v a. Vectors v a => (a -> a -> a) -> v a -> v a
{-# INLINABLE scanl1 #-}
scanl1 f (Slice arr s l)
    | l <= 0    = empty
    | otherwise = case indexArray' arr s of
                    (# x0 #) -> scanl f x0 (Slice arr (s+1) (l-1) :: v a)

{-|
#ifdef CN_DOC
和 'scanl' 方向相反，'scanr' 记录状态值时按照从右往左的顺序。
#else
scanr is the right-to-left dual of scanl.
#endif
-}
scanr :: forall v u a b. (Vectors v a, Vectors u b) => (a -> b -> b) -> b -> v a -> u b
{-# INLINABLE scanr #-}
scanr f z (Slice arr s l) =
    create_ (l+1) (\ marr -> writeMutableArray marr l z >> go z (s+l-1) (l-1) marr)
  where
    go :: b -> Int -> Int -> Mutable (BaseArray u) s b -> ST s ()
    go !acc !i !j !marr
        | j < 0 = return ()
        | otherwise = do
            x <- indexArrayM arr i
            let !acc' = x `f` acc
            writeMutableArray marr j acc'
            go acc' (i-1) (j-1) marr

{-|
#ifdef CN_DOC
'scanr' 的一个变种，使用最后一个元素来作为起始状态。
else
'scanr1' is a variant of 'scanr' that has no starting value argument.
#endif
-}
scanr1 :: forall v a. Vectors v a => (a -> a -> a) -> v a -> v a
{-# INLINABLE scanr1 #-}
scanr1 f (Slice arr s l)
    | l <= 0    = empty
    | otherwise = case indexArray' arr (s+l-1) of
                    (# x0 #) -> scanr f x0 (Slice arr s (l-1) :: v a)


--------------------------------------------------------------------------------
-- Traverse

{-|
#ifdef CN_DOC
遍历切片、连续在 @f@ 函子中执行计算，并把每一步得计算结果记录下来返回新的切片。
#else
Traverse vector and gather result in another vector.
There're rules to optimize the intermedia list away when @f@ is an instance of 'PrimMoand',
such as 'IO', 'ST'.
#endif
>>> traverse (\ x -> \ y -> x + y) ([1..10] :: Vector Integer ) 3 :: Vector Integer -- Reader
[4,5,6,7,8,9,10,11,12,13]
-}
traverse :: (Vectors v a, Vectors u b, Applicative f) => (a -> f b) -> v a -> f (u b)
{-# INLINE [1] traverse #-}
{-# RULES "traverse/ST" forall (f :: a -> ST s b). traverse f = itraversePM (const f) #-}
{-# RULES "traverse/STE" forall (f :: a -> STE e s b). traverse f = itraversePM (const f) #-}
{-# RULES "traverse/IO" forall (f :: a -> IO b). traverse f = itraversePM (const f) #-}
traverse f v = packN (length v) <$> T.traverse f (unpack v)

{-|
#ifdef CN_DOC
和'traverse'类似，但是会给遍历函数额外传递下标参数。
#else
'traverse' with index start from 0.
#endif
-}
itraverse :: (Vectors v a, Vectors u b, Applicative f) => (Int -> a -> f b) -> v a -> f (u b)
{-# INLINE [1] itraverse #-}
{-# RULES "itraverse/ST" forall (f :: Int -> a -> ST s b). itraverse f = itraversePM f #-}
{-# RULES "itraverse/STE" forall (f :: Int -> a -> STE s e b). itraverse f = itraversePM f #-}
{-# RULES "itraverse/IO" forall (f :: Int -> a -> IO b). itraverse f = itraversePM f #-}
itraverse f v = packN (length v) <$> T.traverse (\ (IPair i x) -> f i x) (iunpack v)

{-|
#ifdef CN_DOC
针对 'PrimMonad' 优化的 'itraverse'（因为 PrimMonad 实例中允许数组操作，我们可以直接通过写数组来收集结果），
你可以使用类似下面的 @RULES@ 来优化 'traverse/itraverse' 在你定义的 'PrimMonad' 实例中的性能。
#else
'PrimMonad' specialzied version of 'itraverse'.
You can add rules to rewrite 'traverse' and 'itraverse' to this function in your own 'PrimMonad' instance, e.g.
#endif
@
instance PrimMonad YourMonad where ...
{-# RULES "traverse\/YourMonad" forall (f :: a -> YourMonad b). traverse\' f = itraversePM (const f) #-}
{-# RULES "itraverse\/YourMonad" forall (f :: Int -> a -> YourMonad b). itraverse f = itraversePM f #-}
@
-}
itraversePM 
    :: forall v u a b m. PrimMonad m 
    => (Vectors v a, Vectors u b) => (Int -> a -> m b) -> v a -> m (u b)
{-# INLINE itraversePM #-}
itraversePM f (Slice arr s l)
    | l == 0    = return empty
    | otherwise = do
        !marr <- newMutableArray l
        ba <- go marr 0
        return $! Slice ba 0 l
  where
    go :: Mutable (BaseArray u) (PrimState m) b -> Int -> m (BaseArray u b)
    go marr !i
        | i >= l = unsafeFreezeMutableArray marr
        | otherwise = do
            writeMutableArray marr i =<< f i (indexArray arr (i+s))
            go marr (i+1)

{-|
#ifdef CN_DOC
遍历切片但不收集每一步的结果。
#else
Traverse vector without gathering result.
#endif
>>> traverse_ print ("hello" :: Bytes)
104
101
108
108
111
-}
traverse_ :: (Vectors v a, Applicative f) => (a -> f b) -> v a -> f ()
{-# INLINE traverse_ #-}
traverse_ f (Slice arr s l) = go s
  where
    e = s + l
    go !i
        | i >= e    = pure ()
        | otherwise = f (indexArray arr i) *> go (i+1)

{-|
#ifdef CN_DOC
和'traverse_'类似，但是会给遍历函数额外传递下标参数。
#else
Traverse vector with index.
#endif
>>> itraverse_ (curry print) ("hello" :: Bytes)
(0,104)
(1,101)
(2,108)
(3,108)
(4,111)
-}
itraverse_ :: (Vectors v a, Applicative f) => (Int -> a -> f b) -> v a -> f ()
{-# INLINE itraverse_ #-}
itraverse_ f (Slice arr s l) = go 0
  where
    go !i
        | i >= l    = pure ()
        | otherwise = f i (indexArray arr (i+s)) *> go (i+1)

#ifdef CN_DOC
-- | 'traverse'的别名.
#else
-- | Alias for 'traverse'.
#endif
mapM ::  (Vectors v a, Vectors u b, Applicative f) => (a -> f b) -> v a -> f (u b)
{-# INLINE mapM #-}
mapM = traverse

#ifdef CN_DOC
-- | 'traverse_'的别名.
#else
-- | Alias for 'traverse_'.
#endif
mapM_ ::  (Vectors v a, Applicative f) => (a -> f b) -> v a -> f ()
{-# INLINE mapM_ #-}
mapM_ = traverse_

#ifdef CN_DOC
-- | 'traverse'的参数颠倒版本，某些情况下更便于书写.
#else
-- | Flipped version of 'traverse'.
#endif
forM ::  (Vectors v a, Vectors u b, Applicative f) => v a -> (a -> f b) -> f (u b)
{-# INLINE forM #-}
forM v f = traverse f v

#ifdef CN_DOC
-- | 'traverse_'的参数颠倒版本，某些情况下更便于书写.
#else
-- | Flipped version of 'traverse_'.
#endif
forM_ ::  (Vectors v a, Applicative f) => v a -> (a -> f b) -> f ()
{-# INLINE forM_ #-}
forM_ v f = traverse_ f v

#ifdef CN_DOC
-- | 'itraverse'的别名.
#else
-- | Alias for 'itraverse'.
#endif
imapM ::  (Vectors v a, Vectors u b, Applicative f) => (Int -> a -> f b) -> v a -> f (u b)
{-# INLINE imapM #-}
imapM = itraverse

#ifdef CN_DOC
-- | 'itraverse_'的别名.
#else
-- | Alias for 'itraverse_'.
#endif
imapM_ ::  (Vectors v a, Applicative f) => (Int -> a -> f b) -> v a -> f ()
{-# INLINE imapM_ #-}
imapM_ = itraverse_

#ifdef CN_DOC
-- | 'itraverse'的参数颠倒版本，某些情况下更便于书写.
#else
-- | Flipped version of 'itraverse'.
#endif
iforM ::  (Vectors v a, Vectors u b, Applicative f) => v a -> (Int -> a -> f b) -> f (u b)
{-# INLINE iforM #-}
iforM v f = itraverse f v

#ifdef CN_DOC
-- | 'itraverse_'的参数颠倒版本，某些情况下更便于书写.
#else
-- | Flipped version of 'itraverse_'.
#endif
iforM_ ::  (Vectors v a, Applicative f) => v a -> (Int -> a -> f b) -> f ()
{-# INLINE iforM_ #-}
iforM_ v f = itraverse_ f v

--------------------------------------------------------------------------------
-- Basic creating

{-|
#ifdef CN_DOC
通过'ST'计算创建一个指定长度的切片，并把'ST'计算的结果一同返回。
#else
Create a vector with a initial size N array, return both the vector and
the monadic result during creating.
The result is demanded strictly and the returned vector will be in normal form.
#endif
>>> (create 1 $ \ marr -> do writeMutableArray marr 0 10; return "OK")  :: (String, PrimVector Word16)
("OK",[10])
-}
create
    :: (Vectors v a)
    => Int  -- length in elements of type @a@
    -> (forall s. Mutable (BaseArray v) s a -> ST s b)    -- ^ initialization function
    -> (b, v a)
{-# INLINE create #-}
create n fill = assert (n >= 0) $ runST (do
    marr <- newMutableArray n
    !b <- fill marr
    ba <- unsafeFreezeMutableArray marr
    let !v = Slice ba 0 n
    return (b, v))

{-|
#ifdef CN_DOC
通过'ST'计算创建一个指定长度的切片，忽略'ST'计算结果。
#else
Create a vector with size N with a 'ST' computation.
#endif
-}
create_
    :: (Vectors v a)
    => Int                                   
    -- ^ length in elements of type @a@
    -> (forall s. Mutable (BaseArray v) s a -> ST s ()) 
    -- ^ initialization function
    -> v a
{-# INLINE create_ #-}
create_ n fill = assert (n >= 0) $ runST (do
    marr <- newMutableArray n
    fill marr
    ba <- unsafeFreezeMutableArray marr
    return $! Slice ba 0 n)

{-|
#ifdef CN_DOC
通过'ST'计算创建一个指定长度的切片，但可以让'ST'计算返回一个最终切片长度（必须小于或等于初始给定长度）。
#else
Create a vector up to a specific length.
#endif
-}
createN
    :: Vectors v a
    => Int                                 
    -- ^ length's upper bound
    -> (forall s. Mutable (BaseArray v) s a -> ST s Int)
    -- ^ initialization function which return the actual length
    -> v a
{-# INLINE createN #-}
createN n fill = assert (n >= 0) $ runST (do
    marr <- newMutableArray n
    n' <- fill marr
    shrinkMutableArray marr n'
    ba <- unsafeFreezeMutableArray marr
    return $! Slice ba 0 (min n n'))

#ifdef CN_DOC
-- | /O(1)/. 空切片。
#else
-- | /O(1)/. The empty vector.
#endif
empty :: Vectors v a => v a
{-# NOINLINE empty #-}
empty = Slice emptyArray 0 0

#ifdef CN_DOC
-- | /O(1)/. 单元素切片。
#else
-- | /O(1)/. Single element vector.
#endif
singleton :: Vectors v a => a -> v a
{-# INLINE singleton #-}
singleton x = Slice (singletonArray x) 0 1

#ifdef CN_DOC
-- | /O(1)/. 双元素切片。
#else
-- | /O(1)/. Double element vector.
#endif
doubleton :: Vectors v a => a -> a -> v a
{-# INLINE doubleton #-}
doubleton x y = Slice (doubletonArray x y) 0 2

#ifdef CN_DOC
-- | /O(1)/. 三元素切片。
#else
-- | /O(1)/. Triple element vector.
#endif
tripleton :: Vectors v a => a -> a -> a -> v a
{-# INLINE tripleton #-}
tripleton x y z = Slice (tripletonArray x y z) 0 3

#ifdef CN_DOC
-- | /O(1)/. 拷贝切片，该操作虽然不会有可见的计算影响，但如果之前的切片是很大的数组的很小一部分，
-- 则该操作有可能让之前的切片和数组提前被 GC 回收，从而减少内存使用。
#else
-- | /O(n)/. Copy a vector from slice.
#endif
copy :: Vectors v a => v a -> v a
{-# INLINE copy #-}
copy (Slice ba s l) = create_ l (\ marr -> copyArray marr 0 ba s l)


{-|
#ifdef CN_DOC
类似 'M.replicateM'，重复进行函子计算并把结果保存在一个切片里。
#else
A version of 'replicateM' which works on 'Vectors', with specialized rules under 'PrimMonad'.
#endif
-}
replicateM :: (Applicative f, Vectors v a) => Int -> f a -> f (v a)
{-# INLINE [1] replicateM #-}
{-# RULES "replicateM/ST" forall n (x :: ST s a). replicateM n x = replicatePM n x #-}
{-# RULES "replicateM/STE" forall n (x :: STE s e a). replicateM n x = replicatePM n x #-}
{-# RULES "replicateM/IO" forall n (x :: IO a). replicateM n x = replicatePM n x #-}
replicateM n f = packN n <$> M.replicateM n f

{-|
#ifdef CN_DOC
针对 'PrimMonad' 优化的 'replicateM'，
你可以使用类似下面的 @RULES@ 来优化 'replicateM' 在你定义的 'PrimMonad' 实例中的性能。
#else
'PrimMonad' specialzied version of 'replicateM'.
You can add rules to rewrite 'replicateM' to this function in your own 'PrimMonad' instance, e.g.
#endif
@
instance PrimMonad YourMonad where ...
{-# RULES "replicateM\/YourMonad" forall n (f :: YourMonad a). replicateM n f = replicatePM n f #-}
@
-}
replicatePM :: (PrimMonad m, Vectors v a) => Int -> m a -> m (v a)
{-# INLINE replicatePM #-}
replicatePM n f = do
    !marr <- newMutableArray n
    ba <- go marr 0
    return $! Slice ba 0 n
  where
    go marr !i
        | i >= n = unsafeFreezeMutableArray marr
        | otherwise = do
            x <- f
            writeMutableArray marr i x
            go marr (i+1)

--------------------------------------------------------------------------------
-- Conversion between list

{-|
#ifdef CN_DOC
/O(n)/ 把列表打包成数组切片，等价于 @'packN' 'defaultInitSize'@。
#else
/O(n)/ Convert a list into a vector, alias for @'packN' 'defaultInitSize'@.
#endif
>>> pack [1..10] :: PrimVector Int
[1,2,3,4,5,6,7,8,9,10]
-}
pack :: Vectors v a => [a] -> v a
{-# INLINE pack #-}
pack = packN defaultInitSize

{-|
#ifdef CN_DOC
/O(n)/ 把列表打包成数组切片，第一个参数是一个大小估计，用来帮助优化数组的内存分配，
打包过程中如果发现列表长度超过估计尺寸，则会按照每次翻倍的策略增加。
#else
/O(n)/ Convert a list into a vector with an approximate size.
If the list's length is large than the size given, we simply double the buffer size
and continue building.
This function is a /good consumer/ in the sense of build/foldr fusion.
#endif
>>> packN 10 [1..10] :: PrimVector Int -- allocate once
[1,2,3,4,5,6,7,8,9,10]
>>> packN 4 [1..10] :: PrimVector Int -- allocate three time with size 4, 8, 16 respectively
[1,2,3,4,5,6,7,8,9,10]
-}
packN :: forall v a. Vectors v a => Int -> [a] -> v a
{-# INLINE [1] packN #-}
packN n0 = \ ws0 -> runST (do let n = max 4 n0
                              marr <- newMutableArray n
                              (IPair i marr') <- M.foldM go (IPair 0 marr) ws0
                              shrinkMutableArray marr' i
                              ba <- unsafeFreezeMutableArray marr'
                              return $! Slice ba 0 i)
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: IPair (Mutable (BaseArray v) s a) -> a -> ST s (IPair (Mutable (BaseArray v) s a))
    go (IPair i marr) !x = do
        let i' = i+1
        marr' <- growMutableArray marr i'
        writeMutableArray marr' i x
        return (IPair i' marr')

{-|
#ifdef CN_DOC
/O(n)/ 把列表按照相反的顺序打包成数组切片，等价于 @'packRN' 'defaultInitSize'@。
#else
/O(n)/ Alias for @'packRN' 'defaultInitSize'@.
#endif
>>> packR [1..10] :: PrimVector Int
[10,9,8,7,6,5,4,3,2,1]
-}
packR :: Vectors v a => [a] -> v a
{-# INLINE packR #-}
packR = packRN defaultInitSize
-- $property
-- prop> pack xs == (packR (List.reverse xs) :: Vector Integer)
-- prop> pack xs == reverse (packR xs :: Vector Integer)
-- prop> pack xs == (packR (List.reverse xs) :: PrimVector Int)
-- prop> pack xs == reverse (packR xs :: PrimVector Int)
-- prop> pack xs == (packR (List.reverse xs) :: Bytes)
-- prop> pack xs == reverse (packR xs :: Bytes)

{-|
#ifdef CN_DOC
/O(n)/ 'packN' 的顺序倒转版本。
#else
/O(n)/ 'packN' in reverse order. This function is a /good consumer/ in the sense of build/foldr fusion.
#endif
>>> packRN 10 [1..10] :: PrimVector Int -- allocate once
[10,9,8,7,6,5,4,3,2,1]
>>> packRN 4 [1..10] :: PrimVector Int -- allocate three time with size 4, 8, 16 respectively
[10,9,8,7,6,5,4,3,2,1]
-}
packRN :: forall v a. Vectors v a => Int -> [a] -> v a
{-# INLINE packRN #-}
packRN n0 = \ ws0 -> runST (do let n = max 4 n0
                               marr <- newMutableArray n
                               (IPair i marr') <- M.foldM go (IPair (n-1) marr) ws0
                               ba <- unsafeFreezeMutableArray marr'
                               let i' = i + 1
                                   n' = sizeofArray ba
                               return $! Slice ba i' (n'-i'))
  where
    go :: IPair (Mutable (BaseArray v) s a) -> a -> ST s (IPair (Mutable (BaseArray v) s a))
    go (IPair i marr) !x = do
        !n <- sizeofMutableArray marr
        if i >= 0
        then do writeMutableArray marr i x
                return (IPair (i-1) marr)
        else do let !n' = n `unsafeShiftL` 1  -- double the buffer
                !marr' <- newMutableArray n'
                copyMutableArray marr' n marr 0 n
                writeMutableArray marr' (n-1) x
                return (IPair (n-2) marr')

{-|
#ifdef CN_DOC
/O(n)/ 把切片转换为一个列表，列表在内存中是懒惰生成的。
#else
/O(n)/ Convert vector to a list.
Unpacking is done lazily. i.e. we will retain reference to the array until all element are consumed.
This function is a /good producer/ in the sense of build/foldr fusion.
#endif
>>> let xs = unpack ("hello" :: Bytes)
>>> :sprint xs
xs = _
>>> Prelude.head xs
104
>>> :sprint xs
xs = W8# 104 : _
>>> xs
[104,101,108,108,111]
>>> :sprint xs
xs = [W8# 104,W8# 101,W8# 108,W8# 108,W8# 111]
-}
unpack :: Vectors v a => v a -> [a]
{-# INLINE [1] unpack #-}
unpack (Slice ba s l) = go s
  where
    e = s + l
    go !idx
        | idx >= e  = []
        | otherwise = case indexArray' ba idx of (# x #) -> x : go (idx+1)

unpackFB :: Vectors v a => v a -> (a -> r -> r) -> r -> r
{-# INLINE [0] unpackFB #-}
unpackFB (Slice ba s l) k z = go s
  where
    e = s + l
    go !idx
        | idx >= e  = z
        | otherwise = case indexArray' ba idx of (# x #) -> x `k` go (idx+1)

{-# RULES
"unpack" [~1] forall v . unpack v = build (\ k z -> unpackFB v k z)
"unpackFB" [1] forall v . unpackFB v (:) [] = unpack v
 #-}

{-|
#ifdef CN_DOC
/O(n)/ 'unpack' 的下标版本。
#else
/O(n)/ 'unpack' with element index counting from 0.
#endif
>>> iunpack ("hello" :: Bytes)
[(0,104),(1,101),(2,108),(3,108),(4,111)]
-}
iunpack :: Vectors v a => v a -> [IPair a]
{-# INLINE [1] iunpack #-}
iunpack (Slice ba s l) = go s
  where
    e = s + l
    go !idx
        | idx >= e  = []
        | otherwise = case indexArray' ba idx of (# x #) -> IPair (idx-s) x : go (idx+1)

iunpackFB :: Vectors v a => v a -> (IPair a -> r -> r) -> r -> r
{-# INLINE [0] iunpackFB #-}
iunpackFB (Slice ba s l) k z = go s
  where
    e = s + l
    go !idx
        | idx >= e  = z
        | otherwise = case indexArray' ba idx of (# x #) -> IPair (idx-s) x `k` go (idx+1)

{-# RULES
"iunpack" [~1] forall v . iunpack v = build (\ k z -> iunpackFB v k z)
"iunpackFB" [1] forall v . iunpackFB v (:) [] = iunpack v
 #-}

{-|
#ifdef CN_DOC
/O(n)/ 'unpack' 的倒序版本。
#else
/O(n)/ Convert vector to a list in reverse order.
#endif
>>> unpackR ([1..10] :: PrimVector Int)
[10,9,8,7,6,5,4,3,2,1]
-}
unpackR :: Vectors v a => v a -> [a]
{-# INLINE [1] unpackR #-}
unpackR (Slice ba s l) = go (e - 1)
  where
    e = s + l
    go !idx
        | idx < s = []
        | otherwise =
            case indexArray' ba idx of (# x #) -> x : go (idx-1)

unpackRFB :: Vectors v a => v a -> (a -> r -> r) -> r -> r
{-# INLINE [0] unpackRFB #-}
unpackRFB (Slice ba s l) k z = go (e - 1)
  where
    e = s + l
    go !idx
        | idx < s = z
        | otherwise =
            case indexArray' ba idx of (# x #) -> x `k` go (idx-1)

{-# RULES
"unpackR" [~1] forall v . unpackR v = build (\ k z -> unpackRFB v k z)
"unpackRFB" [1] forall v . unpackRFB v (:) [] = unpackR v
 #-}

{-|
#ifdef CN_DOC
/O(n)/ 'unpackR' 的下标版本（下标从 @-1@ 递减）。
#else
/O(n)/ Convert vector to a list in reverse order with index counting from @-1@.
#endif
>>> iunpackR ([1..10] :: PrimVector Int)
[(-1,10),(-2,9),(-3,8),(-4,7),(-5,6),(-6,5),(-7,4),(-8,3),(-9,2),(-10,1)]
-}
iunpackR :: Vectors v a => v a -> [IPair a]
{-# INLINE [1] iunpackR #-}
iunpackR (Slice ba s l) = go (e - 1)
  where
    e = s + l
    go !idx
        | idx < s = []
        | otherwise =
            case indexArray' ba idx of (# x #) -> IPair (idx-e) x : go (idx-1)

iunpackRFB :: Vectors v a => v a -> (IPair a -> r -> r) -> r -> r
{-# INLINE [0] iunpackRFB #-}
iunpackRFB (Slice ba s l) k z = go (e - 1)
  where
    e = s + l
    go !idx
        | idx < s = z
        | otherwise =
            case indexArray' ba idx of (# x #) -> IPair (idx-e) x `k` go (idx-1)

{-# RULES
"iunpackR" [~1] forall v . iunpackR v = build (\ k z -> iunpackRFB v k z)
"iunpackRFB" [1] forall v . iunpackRFB v (:) [] = iunpackR v
 #-}

--------------------------------------------------------------------------------
-- Basic interface

#ifdef CN_DOC
-- | /O(1)/ 切片长度。
#else
-- | /O(1)/ The length of a vector.
#endif
length :: Vectors v a => v a -> Int
{-# INLINE length #-}
length (Slice _ _ l) = l

#ifdef CN_DOC
-- | /O(1)/ 切片长度是否为0。
#else
-- | /O(1)/ Test whether a vector is empty.
#endif
null :: Vectors v a => v a -> Bool
{-# INLINE null #-}
null (Slice _ _ l) = l == 0

{-|
#ifdef CN_DOC
/O(m+n) 连接两个切片，连接空切片不会申请新的内存。
#else
/O(m+n)/ Concat two slice, appending empty vectors are no-ops.
#endif
>>> "hello" `append` "world" :: Bytes
"helloworld"
-}
append :: Vectors v a => v a -> v a -> v a
{-# INLINE append #-}
append (Slice _ _ 0) b                    = b
append a                (Slice _ _ 0)     = a
append (Slice baA sA lA) (Slice baB sB lB) = create_ (lA+lB) $ \ marr -> do
    copyArray marr 0  baA sA lA
    copyArray marr lA baB sB lB

{-|
#ifdef CN_DOC
/O(n)/ 按顺序连接一个列表中所有的切片。
#else
/O(n)/ Concatenate a list of vector.
#endif
>>> concat ["hello", "world", "good", "bye"] :: Bytes
"helloworldgoodbye"
-}
concat :: forall v a . Vectors v a => [v a] -> v a
{-# INLINABLE concat #-}
concat [v] = v  -- shortcut common case in Parser
concat vs = case preConcat 0 0 vs of
    (1, _) -> case List.find (not . null) vs of Just v -> v -- there must be a not null vector
                                                _ -> error "impossible"
    (_, l) -> create_ l (go vs 0)
  where
    go :: [v a] -> Int -> Mutable (BaseArray v) s a -> ST s ()
    go [] !_ !_ = return ()
    go (Slice ba s l:vs') !i !marr = do
        M.when (l > 0) (copyArray marr i ba s l)
        go vs' (i+l) marr

{-|
#ifdef CN_DOC
/O(n)/ 按从后往前的顺序连接一个列表中所有的切片。
#else
/O(n)/ Concatenate a list of vector in reverse order.
#endif
>>> concatR ["hello", "world", "good", "bye"] :: Bytes
"byegoodworldhello"
-}
concatR :: forall v a . Vectors v a => [v a] -> v a
{-# INLINABLE concatR #-}
concatR [v] = v  -- shortcut common case in Parser
concatR vs = case preConcat 0 0 vs of
    (1, _) -> case List.find (not . null) vs of Just v -> v -- there must be a not null vector
                                                _ -> error "impossible"
    (_, l) -> create_ l (go vs l)
  where
    go :: [v a] -> Int -> Mutable (BaseArray v) s a -> ST s ()
    go [] !_ !_                    = return ()
    go (Slice ba s l:vs') !i !marr = do 
        M.when (l > 0) (copyArray marr (i-l) ba s l)
        go vs' (i-l) marr

-- pre scan to decide if we really need to copy and calculate total length
-- we don't accumulate another result list, since it's rare to got empty
preConcat :: Vectors v a => Int -> Int -> [v a] -> (Int, Int)
{-# INLINE preConcat #-}
preConcat !nacc !lacc [] = (nacc, lacc)
preConcat !nacc !lacc (Slice _ _ l:vs')
    | l == 0    = preConcat nacc lacc vs'
    | otherwise = preConcat (nacc+1) (lacc+l) vs'

{-|
#ifdef CN_DOC
把切片的每一个元素映射成一个切片并相连。
#else
Map a function over a vector and concatenate the results into a vector.
#endif
>>> concatMap (replicate 5) ("hello" :: Bytes) :: Bytes
"hhhhheeeeellllllllllooooo"
-}
concatMap :: (Vectors v a, Vectors u b) => (a -> u b) -> v a -> u b
{-# INLINE concatMap #-}
concatMap f = concat . foldr' ((:) . f) []

--------------------------------------------------------------------------------

{-|
#ifdef CN_DOC
映射切片里的每一个元素，注意输入和返回的切片类型可以是不同的。
#else
Mapping between vectors (possiblely with two different vector types).
#endif
>>> map (replicate 5) ("hello" :: Bytes) :: Vector Bytes 
["hhhhh","eeeee","lllll","lllll","ooooo"]
-}
map :: forall u v a b. (Vectors u a, Vectors v b) => (a -> b) -> u a -> v b
{-# INLINE map #-}
map f (Slice arr s l) = create_ l (go 0)
  where
    go :: Int -> Mutable (BaseArray v) s b -> ST s ()
    go !i !marr | i < l = do
                    x <- indexArrayM arr (i+s)
                    let !v = f x
                    writeMutableArray marr i v
                    go (i+1) marr
                | otherwise = return ()

{-|
#ifdef CN_DOC
'map' 的带下标版本，注意输入和返回的切片类型可以是不同的。
#else
Mapping between vectors with index (possiblely with two different vector types).
#endif
>>> imap replicate ("hello" :: Bytes) :: Vector Bytes 
["","e","ll","lll","oooo"]
-}
imap :: forall u v a b. (Vectors u a, Vectors v b) => (Int -> a -> b) -> u a -> v b
{-# INLINE imap #-}
imap f (Slice arr s l) = create_ l (go 0)
  where
    go :: Int -> Mutable (BaseArray v) s b -> ST s ()
    go !i !marr | i < l = do
                    x <- indexArrayM arr (i+s)
                    let !v = f i x in writeMutableArray marr i v
                    go (i+1) marr
                | otherwise = return ()

{-|
#ifdef CN_DOC
使用 <https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle Fisher-Yates> 算法打乱切片里的元素。
#else
Shuffle a vector using <https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle Fisher-Yates> algorithm.
#endif
>>> g <-System.Random.Stateful.newIOGenM (System.Random.mkStdGen 918413)
>>> shuffle g [1..10] :: IO (PrimVector Int)
[1,4,5,7,3,9,10,6,8,2]
>>> shuffle g [1..10] :: IO (PrimVector Int)
[8,2,9,3,6,5,7,4,1,10]
>>> shuffle g [1..10] :: IO (PrimVector Int)
[2,8,5,1,3,4,7,10,6,9]
-}
shuffle :: (PrimMonad m, StatefulGen g m, Vectors v a) => g -> v a -> m (v a)
{-# INLINE shuffle #-}
shuffle g (Slice arr s l) = do
    marr <- thawArray arr s l
    shuffleMutableArray g marr 0 l
    arr' <- unsafeFreezeMutableArray marr
    pure $! Slice arr' 0 l

{-|
#ifdef CN_DOC
生产切片里元素的全排列。
#else
Generate all permutation of a vector.
#endif
>>> permutations [1..3] :: [PrimVector Int]
[[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
-}
permutations :: forall v a. (Vectors v a) => v a -> [v a]
{-# INLINE permutations #-}
permutations v = packN (length v) <$> List.permutations (unpack v)

--------------------------------------------------------------------------------
--
-- Strict folds

{-|
#ifdef CN_DOC
从左往右折叠（中间步骤严格求值）。
#else
Strict left to right fold.
#endif
>>> foldl' (+) 0 ([1..100] :: PrimVector Int)
5050
-}
foldl' :: Vectors v a => (b -> a -> b) -> b -> v a -> b
{-# INLINE foldl' #-}
foldl' f z (Slice arr s l) = go z s
  where
    e = s + l
    -- tail recursive; traverses array left to right
    go !acc !i | i < e     = case indexArray' arr i of
                                (# x #) -> go (f acc x) (i + 1)
               | otherwise = acc

{-|
#ifdef CN_DOC
从左往右折叠并提供下标（中间步骤严格求值）。
#else
Strict left to right fold with index.
#endif
>>> ifoldl' (\ acc i x -> if even i then x:acc else acc) [] ("helloworld" :: Bytes)
[108,111,111,108,104]
-}
ifoldl' :: Vectors v a => (b -> Int ->  a -> b) -> b -> v a -> b
{-# INLINE ifoldl' #-}
ifoldl' f z (Slice arr s l) = go z s
  where
    e = s + l
    go !acc !i | i < e     = case indexArray' arr i of
                                (# x #) -> go (f acc i x) (i + 1)
               | otherwise = acc

{-|
#ifdef CN_DOC
类似'foldl''，使用起始元素作为初始状态，遇到空切片会报错 'UnexpectedEmptyVector'。
#else
Strict left to right fold using first element as the initial value.
Throw 'UnexpectedEmptyVector' in the case of an empty vector.
#endif
>>> foldl' (+) 0 ([1..100] :: PrimVector Int)
5050
-}
foldl1' :: forall v a. (Vectors v a, HasCallStack) => (a -> a -> a) -> v a -> a
{-# INLINE foldl1' #-}
foldl1' f (Slice arr s l)
    | l == 0    = errorEmptyVector
    | otherwise = case indexArray' arr s of
                    (# x0 #) -> foldl' f x0 (Slice arr (s+1) (l-1) :: v a)

{-|
#ifdef CN_DOC
从右往左折叠（中间步骤严格求值）。
#else
Strict right to left fold.
#endif
>>> foldr' (+) 0 ([1..100] :: PrimVector Int)
5050
-}
foldr' :: Vectors v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr' #-}
foldr' f z (Slice arr s l) = go z (s+l-1)
  where
    -- tail recursive; traverses array right to left
    go !acc !i | i >= s    = case indexArray' arr i of
                                (# x #) -> go (f x acc) (i - 1)
               | otherwise = acc


{-|
#ifdef CN_DOC
/O(n)/ 返回切片里的最大值，
遇到空切片会报错 'UnexpectedEmptyVector'。
#else
/O(n)/ 'maximum' returns the maximum value from a vector, 
an 'UnexpectedEmptyVector' exception will be thrown in the case of an empty vector.
#endif
-}
maximum :: (Vectors v a, Ord a, HasCallStack) => v a -> a
{-# INLINE maximum #-}
maximum = foldl1' max

{-|
#ifdef CN_DOC
/O(n)/ 返回切片里的最小值，
遇到空切片会报错 'UnexpectedEmptyVector'。
#else
/O(n)/ 'minimum' returns the minimum value from a 'vector'
an 'UnexpectedEmptyVector' exception will be thrown in the case of an empty vector.
#endif
-}
minimum :: (Vectors v a, Ord a, HasCallStack) => v a -> a
{-# INLINE minimum #-}
minimum = foldl1' min

{-|
#ifdef CN_DOC
/O(n)/ 返回切片里所有元素的乘积。
#else
/O(n)/ 'product' returns the product value from a vector
#endif
-}
product :: (Vectors v a, Num a) => v a -> a
{-# INLINE product #-}
product = foldl' (*) 1

{-|
#ifdef CN_DOC
/O(n)/ 返回切片里所有元素的和。
#else
/O(n)/ 'sum' returns the sum value from a 'vector'
#endif
-}
sum :: (Vectors v a, Num a) => v a -> a
{-# INLINE sum #-}
sum = foldl' (+) 0

{-|
#ifdef CN_DOC
/O(n)/ 返回切片里是否有元素满足条件。
#else
/O(n)/ Applied to a predicate and a vector, 'any' determines
if any elements of the vector satisfy the predicate.
#endif
-}
any :: Vectors v a => (a -> Bool) -> v a -> Bool
{-# INLINE any #-}
any f (Slice arr s l)
    | l <= 0    = False
    | otherwise = case indexArray' arr s of
                    (# x0 #) -> go (f x0) (s+1)
  where
    !end = s+l
    go !acc !i | acc       = True
               | i >= end  = acc
               | otherwise = case indexArray' arr i of
                                (# x #) -> go (acc || f x) (i+1)

{-|
#ifdef CN_DOC
/O(n)/ 返回切片里是否所有元素都满足条件。
#else
/O(n)/ Applied to a predicate and a vector, 'all' determines
if all elements of the vector satisfy the predicate.
#endif
-}
all :: Vectors v a => (a -> Bool) -> v a -> Bool
{-# INLINE all #-}
all f (Slice arr s l)
    | l <= 0    = True
    | otherwise = case indexArray' arr s of
                    (# x0 #) -> go (f x0) (s+1)
  where
    !end = s+l
    go !acc !i | not acc   = False
               | i >= end  = acc
               | otherwise = case indexArray' arr i of
                                (# x #) -> go (acc && f x) (i+1)


{-|
#ifdef CN_DOC
/O(n)/ 返回切片里某个元素的个数。
#else
/O(n)/ 'count' returns count of an element from a 'vector'
#endif
>>> count (c2w 'l') ("hello" :: Bytes)
2
-}
count :: (Vectors v a, Eq a) => a -> v a -> Int
{-# INLINE[1] count #-}
{-# RULES "count/Bytes" count = countBytes #-}
count w = foldl' (\ acc x -> if x == w then acc+1 else acc) 0

countBytes :: Word8 -> Bytes -> Int
{-# INLINE countBytes #-}
countBytes w8 (PrimVector (PrimArray ba#) s l) =
    unsafeDupablePerformIO (z_count ba# s l w8)

--------------------------------------------------------------------------------
-- Accumulating maps

{-|
#ifdef CN_DOC
'mapAccumL' 结合了 'map' 和 'foldl'，即在折叠的每一步会同时产生新的状态和元素，
函数返回最终的状态以及每一步的新元素。
#else
The 'mapAccumL' function behaves like a combination of 'map' and
'foldl'; it applies a function to each element of a vector,
passing an accumulating parameter from left to right, and returning a
final value of this accumulator together with the new vector.
#endif
>>> mapAccumL (\a b -> (a + b, a)) 0 ([1..10] :: PrimVector Int) :: (Int, PrimVector Int)
(55,[0,1,3,6,10,15,21,28,36,45])
-}
mapAccumL :: forall u v a b c. (Vectors u b, Vectors v c) => (a -> b -> (a, c)) -> a -> u b -> (a, v c)
{-# INLINE mapAccumL #-}
mapAccumL f = imapAccumL (const f)  

{-|
#ifdef CN_DOC
'mapAccumL' 的下标版本。
#else
'mapAccumL' with index.
#endif
-}
imapAccumL :: forall u v a b c. (Vectors u b, Vectors v c) => (Int -> a -> b -> (a, c)) -> a -> u b -> (a, v c)
{-# INLINE imapAccumL #-}
imapAccumL f z (Slice ba s l)
    | l <= 0    = (z, empty)
    | otherwise = create l (go z s)
  where
    !end = s + l
    go :: a -> Int -> Mutable (BaseArray v) s c -> ST s a
    go !acc !i !marr
        | i >= end = return acc
        | otherwise = do
            x <- indexArrayM ba i
            case f (i-s) acc x of
                (!acc', !c) -> do
                    writeMutableArray marr (i-s) c
                    go acc' (i+1) marr

{-|
#ifdef CN_DOC
'mapAccumL' 结合了 'map' 和 'foldr'，即在折叠的每一步会同时产生新的状态和元素，
函数返回最终的状态以及每一步的新元素。
#else
The 'mapAccumL' function behaves like a combination of 'map' and
'foldr'; it applies a function to each element of a vector,
passing an accumulating parameter from right to left, and returning a
final value of this accumulator together with the new vector.
#endif
>>> mapAccumR (\a b -> (a + b, a)) 0 ([1..10] :: PrimVector Int) :: (Int, PrimVector Int)
(55,[54,52,49,45,40,34,27,19,10,0])
-}
mapAccumR :: forall u v a b c. (Vectors u b, Vectors v c) => (a -> b -> (a, c)) -> a -> u b -> (a, v c)
{-# INLINE mapAccumR #-}
mapAccumR f = imapAccumR (const f)  

{-|
#ifdef CN_DOC
'mapAccumR' 的下标版本（下标从@-1@递减）。
#else
'mapAccumR' with index counting down from @-1@.
#endif
-}
imapAccumR :: forall u v a b c. (Vectors u b, Vectors v c) => (Int -> a -> b -> (a, c)) -> a -> u b -> (a, v c)
{-# INLINE imapAccumR #-}
imapAccumR f z (Slice ba s l)
    | l <= 0    = (z, empty)
    | otherwise = create l (go z (end-1))
  where
    !end = s + l
    go :: a -> Int -> Mutable (BaseArray v) s c -> ST s a
    go acc !i !marr
        | i < s     = return acc
        | otherwise = do
            x <- indexArrayM ba i
            case f (i-end) acc x of
                (acc', c) -> do
                    writeMutableArray marr (i-s) c
                    go acc' (i-1) marr

--------------------------------------------------------------------------------
--  Generating and unfolding vector.

{-|
#ifdef CN_DOC
/O(n)/ 复制元素来创建切片，注意这个函数不会对元素本身求值。
#else
/O(n)/ 'replicate' @n x@ is a vector of length @n@ with @x@
the value of every element.
Note: 'replicate' will not force the element in boxed vector case.
#endif
>>> replicate 10 48 :: Bytes
"0000000000"
-}
replicate :: (Vectors v a) => Int -> a -> v a
{-# INLINE replicate #-}
replicate n x | n <= 0    = empty
              | otherwise = create_ n (\ marr -> setMutableArray marr 0 n x)

{-|
#ifdef CN_DOC
/O(n*m)/ 重复切片若干次，等同于 'stimes'。
#else
/O(n*m)/ Repeat a slice n times, alias for 'stimes'.
#endif
>>> cycle 3 "hello" :: Bytes
"hellohellohello"
-}
cycle :: forall v a. Vectors v a => Int -> v a -> v a
{-# INLINE cycle #-}
cycle n (Slice arr s l)
    | l == 0    = empty
    | otherwise = create_ end (go 0)
  where
    !end = n * l
    go :: forall s. Int -> Mutable (BaseArray v) s a -> ST s ()
    go !i !marr | i >= end  = return ()
                | otherwise = copyArray marr i arr s l >> go (i+l) marr

--------------------------------------------------------------------------------
-- Searching element

#ifdef CN_DOC
-- | /O(n)/ 判断一个值在不在切片里。
#else
-- | /O(n)/ 'elem' test if given element is in given vector.
#endif
elem :: (Vectors v a, Eq a) => a -> v a -> Bool
{-# INLINE elem #-}
elem x = isJust . findElem (== x)

-- | /O(n)/ @'not' . 'elem'@
notElem ::  (Vectors v a, Eq a) => a -> v a -> Bool
{-# INLINE notElem #-}
notElem x = not . elem x

{-|
#ifdef CN_DOC
返回切片中所有等于目标值的元素下标。
#else
'elemIndices' returns all the index of matching elements in the vector.
#endif
>>> elemIndices (c2w 'l') ("helloworld" :: Bytes)
[2,3,8]
-}
elemIndices :: Eq a => Vectors v a => a -> v a -> [Int]
{-# INLINE[1] elemIndices #-}
{-# RULES "elemIndices/Bytes1" forall w. elemIndices w = elemIndicesBytes w #-}
elemIndices = elemIndices_

elemIndices_ :: Eq a => Vectors v a => a -> v a -> [Int]
{-# INLINE elemIndices_ #-}
elemIndices_ w (Slice arr s l) = go s
  where
    !end = s + l
    go !p | p >= end  = []
          | otherwise = case indexArray' arr p of 
            (# x #) -> if w == x then (p-s) : go (p+1) else go (p+1)

-- | /O(n)/ Special 'elemIndices' for 'Bytes' using @memchr(3)@
elemIndicesBytes :: Word8 -> Bytes -> [Int]
{-# INLINE elemIndicesBytes #-}
elemIndicesBytes w (PrimVector (PrimArray ba#) s l) = go s
  where
    !end = s + l
    go !i
        | i >= end = []
        | otherwise =
            case z_memchr ba# i w (end - i) of
                -1 -> []
                r  -> let !i' = (i+r) in (i'-s) : go (i'+1)
-- $property
-- prop> elemIndicesBytes w bs == elemIndices_ w bs
-- prop> elemIndicesBytes w bs == List.findIndices (==w) (unpack bs)

{-|
#ifdef CN_DOC
/O(n)/ 返回第一个满足判断条件的元素和下标，如果不存在返回 'Nothing'。
#else
/O(n)/ find the first index and element matching the predicate in a vector
from left to right, if there isn't one, return Nothing.
#endif
>>> findElem (== c2w 'l') ("helloworld" :: Bytes)
Just (2,108)
-}
findElem :: Vectors v a => (a -> Bool) -> v a -> Maybe (IPair a)
{-# INLINE [1] findElem #-}
{-# RULES "findElem/Bytes1" forall w. findElem (w `eqWord8`) = findByte w #-}
{-# RULES "findElem/Bytes2" forall w. findElem (`eqWord8` w) = findByte w #-}
findElem = findElem_

findElem_ :: Vectors v a => (a -> Bool) -> v a -> Maybe (IPair a)
{-# INLINE findElem_ #-}
findElem_ f (Slice arr s l) = go s
  where
    !end = s + l
    go !p 
        | p >= end  = Nothing
        | otherwise = case indexArray' arr p of 
            (# x #) -> if f x  then let !i = p-s in Just (IPair i x) else go (p+1)

-- | /O(n)/ Special 'findByte' for 'Word8' using @memchr(3)@
findByte :: Word8 -> Bytes -> Maybe (IPair Word8)
{-# INLINE findByte #-}
findByte w (PrimVector (PrimArray ba#) s l) =
    case z_memchr ba# s w l of
        -1 -> Nothing
        r  -> Just (IPair r w)
-- $property
-- prop> findElem_  (==w) bs == findByte w bs
-- prop> ifst `fmap` findByte w bs == List.findIndex (==w) (unpack bs)

{-|
#ifdef CN_DOC
/O(n)/ 返回倒数第一个满足判断条件的元素和下标（从 @-1@ 倒数），如果不存在返回 'Nothing'。
#else
/O(n)/ find the first index(counting down from @-1@) and element matching the predicate 
in a vector from right to left, if there isn't one, return Nothing.
#endif
>>> findElemR (== c2w 'l') ("helloworld" :: Bytes)
Just (-2,108)
-}
findElemR :: Vectors v a => (a -> Bool) -> v a -> Maybe (IPair a)
findElemR = findElemR_
{-# INLINE [1] findElemR #-}
{-# RULES "findElemR/Bytes1" forall w. findElemR (w `eqWord8`) = findByteR w #-}
{-# RULES "findElemR/Bytes2" forall w. findElemR (`eqWord8` w) = findByteR w #-}

findElemR_ :: Vectors v a => (a -> Bool) -> v a -> Maybe (IPair a)
{-# INLINE findElemR_ #-}
findElemR_ f (Slice arr s l) = go (s+l-1)
  where
    go !p | p < s     = Nothing
          | otherwise = case indexArray' arr p of 
                (# x #) -> if f x  then let !i = p-s-l in Just (IPair i x) else go (p-1)

-- | /O(n)/ Special 'findElemR' for 'Bytes' with @memrchr@.
findByteR :: Word8 -> Bytes -> Maybe (IPair Word8)
{-# INLINE findByteR #-}
findByteR w (PrimVector (PrimArray ba#) s l) =
    case z_memrchr ba# s w l of
        -1 -> Nothing
        r  -> Just (IPair (r - l) w)
-- $property
-- prop> findElemR_  (==w) bs == findByteR w bs

{-|
#ifdef CN_DOC
/O(n)/ 寻找所有满足条件得元素和下标。
#else
/O(n)/ Find all indices and elements matching the predicate.
#endif
>>> findAllElem (> 10) ([5,6,16,213,4,51,41,43,5,1] :: Vector Integer)
[(2,16),(3,213),(5,51),(6,41),(7,43)]
-}
findAllElem :: Vectors v a => (a -> Bool) -> v a -> [IPair a]
{-# INLINE [1] findAllElem #-}
{-# RULES "findAllElem/Bytes1" forall w. findAllElem (w `eqWord8`) = findAllBytes w #-}
{-# RULES "findAllElem/Bytes2" forall w. findAllElem (`eqWord8` w) = findAllBytes w #-}
findAllElem = findAllElem_

findAllElem_ :: Vectors v a => (a -> Bool) -> v a -> [IPair a]
{-# INLINE findAllElem_ #-}
findAllElem_ f (Slice arr s l) = go s
  where
    !end = s + l
    go !p | p >= end  = []
          | otherwise = case indexArray' arr p of 
                (# x #) -> if f x then IPair (p-s) x : go (p+1) else go (p+1)

-- | /O(n)/ Special 'elemIndices' for 'Bytes' using @memchr(3)@
findAllBytes :: Word8 -> Bytes -> [IPair Word8]
{-# INLINE findAllBytes #-}
findAllBytes w (PrimVector (PrimArray ba#) s l) = go s
  where
    !end = s + l
    go !i
        | i >= end = []
        | otherwise =
            case z_memchr ba# i w (end - i) of
                -1 -> []
                r  -> let !i' = (i+r) in IPair (i'-s) w : go (i'+1)
-- $property
-- prop> findAllElem_  (==w) bs == findAllBytes w bs
-- prop> ifst `fmap` findAllBytes w bs == List.findIndices (==w) (unpack bs)


--------------------------------------------------------------------------------
-- Sub vector search

{-|
#ifdef CN_DOC
/O(n+m)/ 'indices' 的一个变种，会考虑 @haystack@ 中出现 @needle@ 重叠的情况。
#else
/O(n+m)/ Similar to 'indices', but consider overlapping cases of @needle@
within @haystack@ using KMP algorithm.
#endif
>>> indicesOverlapping True "ada" ("adadad" :: Bytes)  -- set partial match to 'True'
[0,2,-2]
>>> indicesOverlapping False "ada" ("adadad" :: Bytes)
[0,2]
>>> indicesOverlapping False "" ("adadad" :: Bytes) -- Empty pattern will return every possible index
[0,1,2,3,4,5]
-}
indicesOverlapping
    :: (Vectors v a, Eq a)
    => Bool -- ^ report partial match at the end of haystack
    -> v a -- ^ vector to search for (@needle@)
    -> v a -- ^ vector to search in (@haystack@)
    -> [Int]
{-# INLINE [1] indicesOverlapping #-}
{-# RULES "indicesOverlapping/Bytes" indicesOverlapping = indicesOverlappingBytes #-}
indicesOverlapping = indicesOverlapping_

indicesOverlapping_ :: (Vectors v a, Eq a) => Bool -> v a -> v a -> [Int]
{-# INLINE indicesOverlapping_ #-}
indicesOverlapping_ reportPartial needle@(Slice narr noff nlen) haystack@(Slice harr hoff hlen) 
    | nlen <= 0 = [0..hlen-1]
    | nlen == 1 = case indexArray' narr noff of
                   (# x #) -> elemIndices x haystack
    | otherwise = kmp 0 0
  where
    next = kmpNextTable needle
    kmp !i !j | i >= hlen = if reportPartial && j /= 0 then [-j] else []
              | narr `indexArray` (j+noff) == harr `indexArray` (i+hoff) =
                    let !j' = j+1
                    in if j' >= nlen
                    then let !i' = i-j
                        in case next `indexArray` j' of
                            -1 -> i' : kmp (i+1) 0
                            j'' -> i' : kmp (i+1) j''
                    else kmp (i+1) j'
              | otherwise = case next `indexArray` j of
                                -1 -> kmp (i+1) 0
                                j' -> kmp i j'

indicesOverlappingBytes :: Bool -> Bytes -> Bytes -> [Int]
{-# INLINE indicesOverlappingBytes #-}
indicesOverlappingBytes reportPartial needle@(Slice narr noff nlen) haystack@(Slice harr hoff hlen)
    | nlen <= 0 = [0..hlen-1]
    | nlen == 1 = elemIndices (indexArray narr noff) haystack
    | otherwise = if popCount bloom > 48 then kmp 0 0 else sunday 0 0
  where
    next = kmpNextTable needle
    bloom = sundayBloom needle
    kmp !i !j | i >= hlen = if reportPartial && j /= 0 then [-j] else []
              | narr `indexArray` (j+noff) == harr `indexArray` (i+hoff) =
                    let !j' = j+1
                    in if j' >= nlen
                    then let !i' = i-j
                        in case next `indexArray` j' of
                            -1 -> i' : kmp (i+1) 0
                            j'' -> i' : kmp (i+1) j''
                    else kmp (i+1) j'
              | otherwise = case next `indexArray` j of
                                -1 -> kmp (i+1) 0
                                j' -> kmp i j'
    hlen' = hlen - nlen
    sunday !i !j | i >= hlen' = kmp i j
                 | narr `indexArray` (j+noff) == harr `indexArray` (i+hoff) =
                        let !j' = j+1
                        in if j' >= nlen
                        then let !i' = i-j
                            in case next `indexArray` j' of
                                -1 -> i' : sunday (i+1) 0
                                j'' -> i' : sunday (i+1) j''
                        else sunday (i+1) j'
                 | otherwise = let !k = i+nlen-j
                                   !afterNeedle = indexArray harr (k+hoff)
                               in if elemSundayBloom bloom afterNeedle
                                  -- fallback to KMP
                                  then case next `indexArray` j of
                                           -1 -> sunday (i+1) 0
                                           j' -> sunday i j'
                                  -- sunday's shifting
                                  else sunday (k+1) 0
-- $property
-- prop> indicesOverlappingBytes partial n h == indicesOverlapping_ partial n h 


{-|
#ifdef CN_DOC
/O(n+m)/ 使用 KMP 算法寻找 @haystack@ 中所有出现过的不重叠的 @needle@ 的下标。
KMP 算法需要以 /O(m)/ 的时间提前计算一个偏移距离表，然后以最差 /O(n+m)/ 的时间复杂度完成搜索。

'indices' 会使用 GHC 改写规则来加速 'Bytes' 的搜索：除了 KMP 偏移的规则之外，我们会针对 @needle@
中所有出现的字节计算一个 64bits 的布隆过滤器，来快速判断当前匹配失败之后，
下一个位置的字节是否出现在 @needle@ 中，来达成 /O(m\/n)/ 的平均复杂度！

你可以通过第一个参数 @report partial match@ 来支持结尾处的部分匹配（返回负下标），参考下面的例子：
#else
/O(n+m)/ Find the offsets of all indices (no-overlapping) of @needle@
within @haystack@ using KMP algorithm.

The KMP algorithm need pre-calculate a shift table in /O(m)/ time and space,
the worst case time complexity for searching is /O(n+m)/.

There's a rewrite rule to accelerate searching in 'Bytes' further by using a
a hybrid algorithm, which need pre-calculate both a shift table in /O(m)/ time and space,
and a bad character bloom filter in /O(m)/ time and /O(1)/ space, the worst case
time complexity is still /O(n+m)/ but the average complexity could be improved to /O(n\/m)/!
See 'sundayBloom' for more details.

Chunked input are support via partial match argument, if set we will return an
extra negative index in case of partial match at the end of input chunk, e.g.
#endif

>>> indices True "ada" ("adadad" :: Bytes)  -- set partial match to 'True'
[0,-2]
>>> indices False "ada" ("adadad" :: Bytes)
[0]
>>> indices False "" ("adadad" :: Bytes) -- Empty pattern will return every possible index
[0,1,2,3,4,5]

References:

* Knuth, Donald; Morris, James H.; Pratt, Vaughan: "Fast pattern matching in strings" (1977)
* <http://www-igm.univ-mlv.fr/~lecroq/string/node8.html#SECTION0080>
* Frantisek FranekChristopher G. JenningsWilliam F. Smyth A Simple Fast Hybrid Pattern-Matching Algorithm (2005)
* D. M. Sunday: A Very Fast Substring Search Algorithm. Communications of the ACM, 33, 8, 132-142 (1990)
* F. Lundh: The Fast Search Algorithm. <http://effbot.org/zone/stringlib.htm> (2006)

/Sometime even myself couldn't understand how this piece of code works and how did i write them,/
/reading notes on 'kmpNextTable' and 'sundayBloom' answer me every time,/
/and remind me Haskell is such a delightful language to work with.  -- Dong /
-}
indices
    :: (Vectors v a, Eq a)
    => Bool -- ^ report partial match at the end of haystack
    -> v a -- ^ vector to search for (@needle@)
    -> v a -- ^ vector to search in (@haystack@)
    -> [Int]
{-# INLINE [1] indices #-}
{-# RULES "indices/Bytes" indices = indicesBytes #-}
indices = indices_

indices_ :: (Vectors v a, Eq a) => Bool -> v a -> v a -> [Int]
{-# INLINE indices_ #-}
indices_ reportPartial needle@(Slice narr noff nlen) haystack@(Slice harr hoff hlen)
    | nlen <= 0 = [0..hlen-1]
    | nlen == 1 = case indexArray' narr noff of
                   (# x #) -> elemIndices x haystack
    | otherwise = kmp 0 0
  where
    next = kmpNextTable needle
    kmp !i !j
        | i >= hlen = if reportPartial && j /= 0 then [-j] else []
        | narr `indexArray` (j+noff) == harr `indexArray` (i+hoff) =
              let !j' = j+1
              in if j' >= nlen
                  then let !i' = i-j in i' : kmp (i+1) 0
                  else kmp (i+1) j'
        | otherwise = case next `indexArray` j of
                          -1 -> kmp (i+1) 0
                          j' -> kmp i j'

-- | /O(n\/m)/ Find the offsets of all non-overlapping indices of @needle@
-- within @haystack@ using KMP algorithm, combined with simplified sunday's
-- rule to obtain /O(m\/n)/ complexity in average use case.
indicesBytes 
    :: Bool -- ^ report partial match at the end of haystack
    -> Bytes -- ^ bytes to search for (@needle@)
    -> Bytes -- ^ bytes to search in (@haystack@)
    -> [Int]
{-# INLINE indicesBytes #-}
indicesBytes reportPartial needle@(Slice narr noff nlen) haystack@(Slice harr hoff hlen)
    | nlen <= 0 = [0..hlen-1]
    | nlen == 1 = case indexArray' narr noff of
                   (# x #) -> elemIndices x haystack
    | otherwise = if popCount bloom > 48 then kmp 0 0 else sunday 0 0
  where
    next = kmpNextTable needle
    bloom = sundayBloom needle
    kmp !i !j | i >= hlen = if reportPartial && j /= 0 then [-j] else []
              | narr `indexArray` (j+noff) == harr `indexArray` (i+hoff) =
                    let !j' = j+1
                    in if j' >= nlen
                        then let !i' = i-j in i' : kmp (i+1) 0
                        else kmp (i+1) j'
              | otherwise = case next `indexArray` j of
                                -1 -> kmp (i+1) 0
                                j' -> kmp i j'
    hlen' = hlen - nlen
    sunday !i !j | i >= hlen' = kmp i j
                 | narr `indexArray` (j+noff) == harr `indexArray` (i+hoff) =
                        let !j' = j+1
                        in if j' >= nlen
                            then let !i' = i-j in i' : sunday (i+1) 0
                            else sunday (i+1) j'
                 | otherwise = let !k = i+nlen-j
                                   !afterNeedle = indexArray harr (k+hoff)
                               in if elemSundayBloom bloom afterNeedle
                                  -- fallback to KMP
                                  then case next `indexArray` j of
                                           -1 -> sunday (i+1) 0
                                           j' -> sunday i j'
                                  -- sunday's shifting
                                  else sunday (k+1) 0
-- $property
-- prop> indicesBytes partial n h == indices_ partial n h 

{-|
#ifdef CN_DOC
/O(m)/ 计算 KMP 偏移表。KMP 算法的偏移规则可以概括为：一旦 @needle[j]@ 和 @haystack[i]@
不相等，检查 @nextTabel[j]@ 是否为 @-1@ （代表 @needle[j]@ 本身在前面的 @needle@ 也没出现过），
如果是则下次比较应该从 @needle[0]@ 和 @haystack[i+1]@ 继续进行。
否则比较应该从 @needle[nextTable[j]]@ 和 @haystack[i]@ 继续进行。

这个版本的 KMP 偏移表支持重叠的 @needle@（见 'indicesOverlapping'），
因此会比 @needle@ 多一位 @j@，代表已经成功找到 @needle@ 之后，
下次比较应该从 @needle[j]@ 和 @haystack[i+1]@ 继续比较。
#else
/O(m)/ Calculate the KMP next shift table.
The shifting rules is: when a mismatch between @needle[j]@ and @haystack[i]@
is found, check if @nextTable[j] == -1@, if so next search continue with @needle[0]@
and @haystack[i+1]@, otherwise continue with @needle[nextTable[j]]@ and @haystack[i]@.

This version also support overlapping indices by putting an extra shifting index at the
end, which is used when a @needle@ is successfully found and we should start comparing
@needle[nextTable[j]]@ and @haystack[i+1]@ so see if there's a overlapping needle.
#endif
>>> kmpNextTable ("hello" :: Bytes)  -- just keep looking for 'h' if haystack[i] != 'e', 'l' ...
fromListN 6 [-1,0,0,0,0,0]
>>> kmpNextTable ("aaaaaa" :: Bytes) -- you should always jump to haystack[i+1] when haystack[i] != 'a'
fromListN 7 [-1,-1,-1,-1,-1,-1,5]
>>> kmpNextTable ("ababc" :: Bytes)  -- when haystack[i] != last 'c', try match haystack[i] with needle[2]
fromListN 6 [-1,0,-1,0,2,0]
-}
kmpNextTable :: (Vectors v a, Eq a) => v a -> PrimArray Int
{-# INLINE kmpNextTable #-}
kmpNextTable (Slice arr s l) = runST (do
    ma <- newMutableArray (l+1)
    writeMutableArray ma 0 (-1)
    let dec !w !j
            | j < 0 || w == indexArray arr (s+j) = return $! j+1
            | otherwise = readMutableArray ma j >>= dec w
        go !i !j
            | i > l    = unsafeFreezeMutableArray ma
            | otherwise = do
                let !w = indexArray arr (s+i-1)
                j' <- dec w j
                if i < l && indexArray arr (s+j') == indexArray arr (s+i)
                    then readMutableArray ma j' >>= writeMutableArray ma i
                    else writeMutableArray ma i j'
                go (i+1) j'
    go 1 (-1))

{-|
#ifdef CN_DOC
/O(m)/ 为 sunday 的偏移法则准备一个简单的布隆过滤器.

Sunday 偏移规则是说，当 @needle[j]@ 和 @haystack[i]@ 不匹配时，通过观察 @haystack[i+n-j]@ 是否在 
@needle@ 中出现过，可以直接跳过后续比较，从 @haystack[i+n-j+1]@ 和 @needle[0]@ 处继续进行匹配。
What a insightful observation!
但是如何快速判断一个元素有没有在 needle 中出现过是一个难题，这里提供一个简单的 64bits 布隆过滤器,
而且仅在置位 <= 48bits 时启用，否则会因为 false positive rate 过高而得不偿失。具体得构造和判断规则是：
对于 @needle@ 中的每一个 'Word8' 执行 @unsafeShiftL 0x01 (w .&. 0x3f)@ 后加入过滤器。
#else
/O(m)/ Calculate a simple bloom filter for simplified sunday's rule.

The shifting rules is: when a mismatch between @needle[j]@ and @haystack[i]@
is found, check if @elemSundayBloom bloom haystack[i+n-j]@, where n is the
length of needle, if not then next search can be safely continued with
@haystack[i+n-j+1]@ and @needle[0]@, otherwise next searh should continue with
@haystack[i]@ and @needle[0]@, or fallback to other shifting rules such as KMP.

The bloom generate algorithm is very simple: for a given 'Word8' @w@, we set the bloom's bit
at @unsafeShiftL 0x01 (w .&. 0x3f)@, so there're three false positives per bit.
This's particularly suitable for search UTF-8 bytes since the significant bits
of a beginning byte is usually the same.
#endif
>>> sundayBloom "helloworld"
37314332270264320
>>> elemSundayBloom 37314332270264320 (c2w 'h')
True
>>> elemSundayBloom 37314332270264320 (c2w 'x')
False
>>> elemSundayBloom 37314332270264320 (c2w '$')     -- false positive
True
-}
sundayBloom :: Bytes -> Word64
{-# INLINE sundayBloom #-}
sundayBloom (Slice arr s l) = go 0x00000000 s
  where
    !end = s+l
    go !b !i
        | i >= end  = b
        | otherwise =
            let !w = indexArray arr i
                !b' = b .|. (0x00000001 `unsafeShiftL` (fromIntegral w .&. 0x3f))
            in go b' (i+1)

#ifdef CN_DOC
-- | O(1) 测试 'Word8' 是否在布隆过滤器里。
#else
-- | O(1) Test if a bloom filter contain a certain 'Word8'.
#endif
elemSundayBloom :: Word64 -> Word8 -> Bool
{-# INLINE elemSundayBloom #-}
elemSundayBloom b w = b .&. (0x01 `unsafeShiftL` (fromIntegral w .&. 0x3f)) /= 0

--------------------------------------------------------------------------------
-- Filter & Partition

{-|
#ifdef CN_DOC
/O(n)/ 使用判断函数过滤切片。
#else
/O(n)/ 'filter', applied to a predicate and a vector,
returns a vector containing those elements that satisfy the
predicate.
#endif
>>> filter (even) ([0..10] :: PrimVector Int)
[0,2,4,6,8,10]
-}
filter :: forall v a. Vectors v a => (a -> Bool) -> v a -> v a
{-# INLINABLE filter #-}
filter f (Slice arr s l)
    | l == 0    = empty
    | otherwise = createN l (go 0 s)
  where
    !end = s + l
    go :: Int -> Int -> Mutable (BaseArray v) s a -> ST s Int
    go !i !p !marr
        | p >= end    = return i
        | f x         = writeMutableArray marr i x >> go (i+1) (p+1) marr
        | otherwise   = go i (p+1) marr
        where (# x #) = indexArray' arr p

{-|
#ifdef CN_DOC
/O(n)/ 使用判断函数把切片分成满足和不满足的两部分。
#else
/O(n)/ The 'partition' function takes a predicate, a vector, returns
a pair of vector with elements which do and do not satisfy the
predicate, respectively; i.e.,
#endif
>>> partition (even) ([0..10] :: PrimVector Int)
([0,2,4,6,8,10],[1,3,5,7,9])
-}
partition :: forall v a. Vectors v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINABLE partition #-}
partition g (Slice arr s l)
    | l == 0    = (empty, empty)
    | otherwise = runST $ do
        marr0 <- newMutableArray l
        marr1 <- newMutableArray l
        go 0 0 s marr0 marr1
  where
    !end = s + l
    go :: Int -> Int -> Int -> Mutable (BaseArray v) s a -> Mutable (BaseArray v) s a -> ST s (v a, v a)
    go !i !j !p !mba0 !mba1
        | p >= end   = do
                        shrinkMutableArray mba0 i
                        shrinkMutableArray mba1 j
                        ba0 <- unsafeFreezeMutableArray mba0
                        ba1 <- unsafeFreezeMutableArray mba1
                        return (Slice ba0 0 i, Slice ba1 0 j)
        | g x        = writeMutableArray mba0 i x >> go (i+1) j (p+1) mba0 mba1
        | otherwise  = writeMutableArray mba1 j x >> go i (j+1) (p+1) mba0 mba1
        where (# x #) = indexArray' arr p
-- $property
-- prop> \ (Fun _ p) -> partition p (vs :: Vector Integer) == (filter p vs, filter (not . p) vs)
-- prop> \ (Fun _ p) -> partition p (vs :: PrimVector Int) == (filter p vs, filter (not . p) vs)
-- prop> \ (Fun _ p) -> partition p (vs :: Bytes) == (filter p vs, filter (not . p) vs)

--------------------------------------------------------------------------------
-- Comparison Sort

{-|
#ifdef CN_DOC
/O(n*log(n))/ 使用<https://en.wikipedia.org/wiki/Merge_sort 归并算法>排序切片。
#else
/O(n*log(n))/ Sort vector based on element's 'Ord' instance with classic
<https://en.wikipedia.org/wiki/Merge_sort mergesort> algorithm.
This is a stable sort, During sorting two O(n) worker arrays are needed, one of
them will be freezed into the result vector.
#endif
>>> mergeSort "helloworld" :: Bytes
"dehllloorw"
-}
mergeSort :: forall v a. (Vectors v a, Ord a) => v a -> v a
{-# INLINE mergeSort #-}
mergeSort = mergeSortBy compare
-- $property
-- prop> \ (xs :: Vector Integer) -> unpack (mergeSort xs) == List.sort (unpack xs)
-- prop> \ (xs :: PrimVector Int) -> unpack (mergeSort xs) == List.sort (unpack xs)
-- prop> \ (xs :: Bytes) -> unpack (mergeSort xs) == List.sort (unpack xs)

{-|
#ifdef CN_DOC
/O(n*log(n))/ 手动传递比较函数的归并排序。
#else
/O(n*log(n))/ Sort vector based on comparing function using merge sort.
#endif
>>> mergeSortBy (flip compare) "helloworld" :: Bytes
"wroolllhed"
-}
mergeSortBy :: forall v a. Vectors v a => (a -> a -> Ordering) -> v a -> v a
{-# INLINE mergeSortBy #-}
mergeSortBy cmp (Slice arr s l) = runST $ do
    -- create two worker array
    w1 <- newMutableArray l
    w2 <- newMutableArray l
    firstPass w1
    w <- mergePass w1 w2 2
    return $! Slice w 0 l
  where
    firstPass :: forall s. Mutable (BaseArray v) s a -> ST s ()
    firstPass !marr = do
        let !l' = l-1 
            go !i = do
                M.when (i < l') $ do
                    let i' = i + s
                    x <- indexArrayM arr i'
                    y <- indexArrayM arr (i'+1)
                    case inline cmp x y of
                        LT -> writeMutableArray marr i x >> writeMutableArray marr (i+1) y
                        _  -> writeMutableArray marr i y >> writeMutableArray marr (i+1) x
                    go (i+2)
        go 0
        -- if slice have odd length, copy leftover
        M.when (odd l) $ writeMutableArray marr l' =<< indexArrayM arr (s+l')

    mergePass :: forall s. Mutable (BaseArray v) s a -> Mutable (BaseArray v) s a -> Int -> ST s (BaseArray v a)
    mergePass !w1 !w2 !blkSiz
        | blkSiz >= l = unsafeFreezeMutableArray w1
        | otherwise     = do
            let loopOuter !blkStart =
                    if blkStart >= l-blkSiz
                    -- remaining elements less than a block
                    then M.when (blkStart < l) $ copyMutableArray w2 blkStart w1 blkStart (l-blkStart)
                    else do
                        let !leftEnd = blkStart + blkSiz
                            !rightEnd = min (leftEnd + blkSiz) l
                            loopInner !i !j !k = do
                                !lv <- readMutableArray w1 i
                                !rv <- readMutableArray w1 j
                                case inline cmp rv lv of
                                    LT -> do
                                        writeMutableArray w2 k rv
                                        let !j' = j + 1
                                            !k' = k + 1
                                        if j' >= rightEnd
                                        then copyMutableArray w2 k' w1 i (leftEnd - i)
                                        else loopInner i j' k'
                                    _ -> do
                                        writeMutableArray w2 k lv
                                        let !i' = i + 1
                                            !k' = k + 1
                                        if i' >= leftEnd
                                        then copyMutableArray w2 k' w1 j (rightEnd - j)
                                        else loopInner i' j k'
                        loopInner blkStart leftEnd blkStart
                        loopOuter rightEnd
            loopOuter 0
            mergePass w2 w1 (blkSiz*2) -- swap worker array and continue merging

--------------------------------------------------------------------------------
-- Radix Sort

{-|
#ifdef CN_DOC
能够分解成为基数的类型，默认的实例统一按照字节顺序，占用大小为 @256@ 的 bucket。
#else
Types contain radixs, which can be inspected with 'radix' during different 'passes'.
The default instances share a same 'bucketSize' 256, which seems to be a good default.
#endif
-}
class Radix a where
    -- | The size of an auxiliary array, i.e. the counting bucket
    bucketSize :: a -> Int
    -- | The number of passes necessary to sort an array of es.
    passes :: a -> Int
    -- | The radix function parameterized by the current pass (0 <= pass < passes e).
    radix  :: Int -> a -> Int

instance Radix Int8 where
    {-# INLINE bucketSize #-};
    bucketSize _ = 256
    {-# INLINE passes #-}
    passes _ = 1
    {-# INLINE radix #-}
    radix _ a =  255 .&. fromIntegral a `xor` 128

#define MULTI_BYTES_INT_RADIX(T, msb) \
    {-# INLINE bucketSize #-}; \
    bucketSize _ = 256; \
    {-# INLINE passes #-}; \
    passes _ = sizeOf (undefined :: T); \
    {-# INLINE radix #-}; \
    radix 0 a = fromIntegral (255 .&. a); \
    radix msb a = fromIntegral ((a `xor` minBound) `unsafeShiftR` (msb `unsafeShiftL` 3)) .&. 255; \
    radix i a = fromIntegral (a `unsafeShiftR` (i `unsafeShiftL` 3)) .&. 255; 

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
instance Radix Int where MULTI_BYTES_INT_RADIX(Int, 3)
#else 
instance Radix Int where MULTI_BYTES_INT_RADIX(Int, 7)
#endif
instance Radix Int16 where MULTI_BYTES_INT_RADIX(Int16, 1)
instance Radix Int32 where MULTI_BYTES_INT_RADIX(Int32, 3)
instance Radix Int64 where MULTI_BYTES_INT_RADIX(Int64, 7)

instance Radix Word8 where
    {-# INLINE bucketSize #-};
    bucketSize _ = 256
    {-# INLINE passes #-}
    passes _ = 1
    {-# INLINE radix #-}
    radix _  = fromIntegral

#define MULTI_BYTES_WORD_RADIX(T) \
    {-# INLINE bucketSize #-}; \
    bucketSize _ = 256; \
    {-# INLINE passes #-}; \
    passes _ = sizeOf (undefined :: T); \
    {-# INLINE radix #-}; \
    radix 0 a = fromIntegral (255 .&. a); \
    radix i a = fromIntegral (a `unsafeShiftR` (i `unsafeShiftL` 3)) .&. 255; 

instance Radix Word where MULTI_BYTES_WORD_RADIX(Word)
instance Radix Word16 where MULTI_BYTES_WORD_RADIX(Word16)
instance Radix Word32 where MULTI_BYTES_WORD_RADIX(Word32)
instance Radix Word64 where MULTI_BYTES_WORD_RADIX(Word64)

{- |
#ifdef CN_DOC
类似 'Down' 和 'Ord' 的关系，'RadixDown' 可以用来反转 'Radix' 的排序顺序。
#else
Similar to 'Down' newtype for 'Ord', this newtype can inverse the order of a 'Radix'
instance when used in 'radixSort'.
#endif
>>> radixSort [RadixDown 34, RadixDown 12, RadixDown 13, RadixDown 113, RadixDown 89] :: PrimVector (RadixDown Int)
[RadixDown 113,RadixDown 89,RadixDown 34,RadixDown 13,RadixDown 12]
-}
newtype RadixDown a = RadixDown { getRadixDown :: a }
    deriving newtype (Eq, Prim, PrimUnlifted)

instance (Show a) => Show (RadixDown a) where
    showsPrec d (RadixDown x) = showParen (d > 10) $
        showString "RadixDown " . showsPrec 11 x

instance Radix a => Radix (RadixDown a) where
    {-# INLINE bucketSize #-}
    bucketSize (RadixDown a) = bucketSize a
    {-# INLINE passes #-}
    passes (RadixDown a)  = passes a
    {-# INLINE radix #-}
    radix i (RadixDown a) = bucketSize a - radix i a -1

{-|
#ifdef CN_DOC
/O(n)/ 根据元素的 'Radix' 定义来进行<https://en.wikipedia.org/wiki/Radix_sort 基数排序>，
这个算法特别适合像 'Word8' 或者 'Int8' 这样的小尺寸类型，当类型的字节数较多，而切片较短时，
该算法的效率会因为固定 pass 而下降，一般来说当切片尺寸小于 @2^(2*passes)@ 时不如 'mergeSort'。
#else
/O(n)/ Sort vector based on element's 'Radix' instance with
<https://en.wikipedia.org/wiki/Radix_sort radix-sort>,
(Least significant digit radix sorts variation).

This is a stable sort, one or two extra O(n) worker array are need
depend on how many 'passes' shall be performed, and a 'bucketSize'
counting bucket are also needed. This sort algorithms performed extremly
well on small byte size types such as 'Int8' or 'Word8', while on larger
type, constant passes may render this algorithm not suitable for small
vectors (turning point around @2^(2*passes)@).
#endif
-}
radixSort :: forall v a. (Vectors v a, Radix a) => v a -> v a
{-# INLINABLE radixSort #-}
radixSort v@(Slice arr s l)
    | l <= 1 = v
    | otherwise = runST (do
        bucket <- newMutableArrayWith buktSiz 0 :: ST s (MutablePrimArray s Int)
        w1 <- newMutableArray l :: ST s (Mutable (BaseArray v) s a)
        countPass0 arr bucket s
        accumPass bucket buktSiz 0 0
        movePass0 arr bucket w1 s
        w <- if passSiz == 1
            then unsafeFreezeMutableArray w1
            else do
                w2 <- newMutableArray l :: ST s (Mutable (BaseArray v) s a)
                radixLoop w1 w2 bucket buktSiz 1
        return $! Slice w 0 l)
  where
    passSiz = passes (undefined :: a)
    buktSiz = bucketSize (undefined :: a)
    !end = s + l

    countPass0 :: forall s. BaseArray v a -> MutablePrimArray s Int -> Int -> ST s ()
    countPass0 !arr' !bucket !i
        | i >= end  = return ()
        | otherwise = case indexArray' arr' i of
            (# x #) -> do
                let !r = radix 0 x
                c <- readMutableArray bucket r
                writeMutableArray bucket r (c+1)
                countPass0 arr' bucket (i+1)

    movePass0 :: forall s. BaseArray v a -> MutablePrimArray s Int -> Mutable (BaseArray v) s a -> Int -> ST s ()
    movePass0 !arr' !bucket !w !i
        | i >= end  = return ()
        | otherwise = case indexArray' arr' i of
            (# x #) -> do
                let !r = radix 0 x
                c <- readMutableArray bucket r
                writeMutableArray bucket r (c+1)
                writeMutableArray w c x
                movePass0 arr' bucket w (i+1)

    accumPass :: forall s. MutablePrimArray s Int -> Int -> Int -> Int -> ST s ()
    accumPass !bucket !bsiz !i !acc
        | i >= bsiz = return ()
        | otherwise = do
            c <- readMutableArray bucket i
            writeMutableArray bucket i acc
            accumPass bucket bsiz (i+1) (acc+c)

    radixLoop :: forall s. Mutable (BaseArray v) s a -> Mutable (BaseArray v) s a -> MutablePrimArray s Int -> Int -> Int -> ST s ((BaseArray v) a)
    radixLoop !w1 !w2 !bucket !bsiz !pass = do
        setMutableArray bucket 0 bsiz 0   -- clear the counting bucket
        countPass w1 bucket pass 0
        accumPass bucket bsiz 0 0
        movePass w1 bucket pass w2 0
        if pass >= passSiz-1 
        then unsafeFreezeMutableArray w2
        else radixLoop w2 w1 bucket bsiz (pass+1)

    countPass :: forall s. Mutable (BaseArray v) s a -> MutablePrimArray s Int -> Int -> Int -> ST s ()
    countPass !src !bucket !pass !i
        | i >= l  = return ()
        | otherwise = do
                x <- readMutableArray src i
                let !r = radix pass x
                c <- readMutableArray bucket r
                writeMutableArray bucket r (c+1)
                countPass src bucket pass (i+1)

    movePass :: forall s. Mutable (BaseArray v) s a -> MutablePrimArray s Int -> Int -> Mutable (BaseArray v) s a -> Int -> ST s ()
    movePass !src !bucket !pass !target !i
        | i >= l  = return ()
        | otherwise = do
                x <- readMutableArray src i
                let !r = radix pass x
                c <- readMutableArray bucket r
                writeMutableArray bucket r (c+1)
                writeMutableArray target c x
                movePass src bucket pass target (i+1)
-- $property
-- prop> \ (xs :: Vector Int) -> unpack (radixSort xs) == List.sort (unpack xs)
-- prop> \ (xs :: PrimVector Int) -> unpack (radixSort xs) == List.sort (unpack xs)
-- prop> \ (xs :: Bytes) -> unpack (radixSort xs) == List.sort (unpack xs)

--------------------------------------------------------------------------------
-- | merge duplicated adjacent element, prefer left element.

{-|
#ifdef CN_DOC
合并相邻的相同元素，结合排序算法可以得到类似 'nub' 的效果（但复杂度大大降低）。
#else
Use this function with sort will have the same effects as 'nub', but in less complexity.
#endif
>>> mergeAdjacent (mergeSort "mergeAdjacent = mergeAdjacentBy (==) const") :: Bytes
" ()=ABacdegjmnorsty"
-}
mergeAdjacent :: forall v a. (Vectors v a, Eq a) => v a -> v a
{-# INLINABLE mergeAdjacent #-}
mergeAdjacent = mergeAdjacentBy (==) const

{-|
#ifdef CN_DOC
合并相邻的相同元素，提供判断函数和合并函数（上一步的合并结果会作为新的判断、合并参数）。
#else
Merge duplicated adjacent element, based on a equality tester and a merger function.
The merger function will be applied with result from previous step and next element.
#endif
-}
mergeAdjacentBy :: forall v a. Vectors v a
                   => (a -> a -> Bool)  -- ^ equality tester, @\ left right -> eq left right@
                   -> (a -> a -> a)     -- ^ the merger, @\ left right -> merge left right@
                   -> v a -> v a
{-# INLINABLE mergeAdjacentBy #-}
mergeAdjacentBy eq merger v@(Slice arr s l)
    | l == 0 = empty
    | l == 1 = v
    | otherwise =
        let i = findFirstDup (s+1) (indexArray arr s)
        in if i == end
            then v
            else createN l $ \ marr -> do
                let noDupLen = i - s
                M.when (noDupLen > 0) (copyArray marr 0 arr s noDupLen)
                x0 <- indexArrayM arr i
                x1 <- indexArrayM arr (i+1)
                let !x' = merger x0 x1
                writeMutableArray marr noDupLen x'
                go marr (i+2) (noDupLen+1) x'
  where
    !end = s + l

    findFirstDup :: Int -> a -> Int
    findFirstDup !i !x
        | i >= end = i
        | otherwise =
            case indexArray' arr i of
                (# x' #) ->
                    if x `eq` x'
                    then (i-1)
                    else findFirstDup (i+1) x'

    go :: forall s. Mutable (BaseArray v) s a -> Int -> Int -> a -> ST s Int
    go !marr !i !j !x
        | i >= end  = return j
        | otherwise = do
            x' <- indexArrayM arr i
            if x `eq` x'
            then do
                let !x'' = merger x x'
                writeMutableArray marr (j-1) x''
                go marr (i+1) j x''
            else do
                writeMutableArray marr j x'
                go marr (i+1) (j+1) x'

--------------------------------------------------------------------------------

#ifdef CN_DOC
-- | 帮助 GHC unpack 的数据类型，在写一些循环时有用。
#else
-- | Pair type to help GHC unpack in some loops, useful when write fast folds.
#endif
data IPair a = IPair { ifst :: {-# UNPACK #-}!Int, isnd :: !a } deriving (Eq, Ord)

-- | @(i, x)@
instance Show a => Show (IPair a) where
  showsPrec _ (IPair ix a) = ('(' :) . shows ix . (',':) . shows a . (')':)

instance (Arbitrary v) => Arbitrary (IPair v) where
    arbitrary = toIPair <$> arbitrary
    shrink v = toIPair <$> shrink (fromIPair v)

instance (CoArbitrary v) => CoArbitrary (IPair v) where
    coarbitrary = coarbitrary . fromIPair

instance Functor IPair where
    {-# INLINE fmap #-}
    fmap f (IPair i v) = IPair i (f v)

instance NFData a => NFData (IPair a) where
    {-# INLINE rnf #-}
    rnf (IPair _ a) = rnf a

-- | Convert to a tuple.
fromIPair :: IPair a -> (Int, a)
{-# INLINE fromIPair #-}
fromIPair (IPair i v) = (i, v)

-- | Convert from a tuple.
toIPair :: (Int, a) -> IPair a
{-# INLINE toIPair #-}
toIPair (i, v) = IPair i v


#ifdef CN_DOC
-- | 默认的 chunk 尺寸， @defaultChunkSize = 16k - chunkOverhead@
#else
-- | The chunk size used for I\/O. Currently set to @16k - chunkOverhead@
#endif
defaultChunkSize :: Int
{-# INLINE defaultChunkSize #-}
defaultChunkSize = 16 * 1024 - chunkOverhead

#ifdef CN_DOC
-- | 较小的 chunk 尺寸， @smallChunkSize = 16k - chunkOverhead@
#else
-- | The recommended chunk size. Currently set to @4k - chunkOverhead@.
#endif
smallChunkSize :: Int
{-# INLINE smallChunkSize #-}
smallChunkSize = 4 * 1024 - chunkOverhead

#ifdef CN_DOC
-- | GHC 原始类型数组对象的 meta 尺寸， @chunkOverhead == 2 * wordsSize@
#else
-- | The memory management overhead. Currently this is tuned for GHC only.
#endif
chunkOverhead :: Int
{-# INLINE chunkOverhead #-}
chunkOverhead = 2 * sizeOf (undefined :: Int)


#ifdef CN_DOC
-- | @defaultInitSize = 30@，用在诸如 'pack', 'packR' 等不确定结果长度的地方。
#else
-- | @defaultInitSize = 30@, used as initialize size when packing list of unknown size.
#endif
defaultInitSize :: Int
{-# INLINE defaultInitSize #-}
defaultInitSize = 30

#ifdef CN_DOC
-- | 一些切片操作中可能出发的异常。
#else
-- | All exception can be throw by using 'Vectors'.
#endif
data VectorException = IndexOutOfVectorRange {-# UNPACK #-} !Int CallStack
                     | UnexpectedEmptyVector CallStack
                    deriving Show
instance Exception VectorException

errorEmptyVector :: HasCallStack => a
{-# INLINE errorEmptyVector #-}
errorEmptyVector = throw (UnexpectedEmptyVector callStack)

errorOutOfRange :: HasCallStack => Int -> a
{-# INLINE errorOutOfRange #-}
errorOutOfRange i = throw (IndexOutOfVectorRange i callStack)
