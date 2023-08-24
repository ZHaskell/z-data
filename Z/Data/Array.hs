#ifdef CN_DOC
{-|
Module      : Z.Data.Array
Description : 统一的数组操作
Copyright   : (c) Dong Han, 2023
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Haskell 的数组类型和大部分语言提供的不太相同：

* Haskell 中并没有提供语法层面的支持，例如类似 @array[3]@ 的操作 在 Haskell 中必须使用 @indexArray array 3@ 来完成
* Haskell 区分可变数组和不可变数组，操作可变数组必须在 'ST', 'IO' 或者其他的 'PrimMonad' 里，因此在使用本模块的函数之前，请确保对 'ST', 'IO' 这类 monad 有正确的理解。
* GHC 区分不同类型元素的数组容器，例如 @'PrimArray' 'Int'@ 和 @'A.Array' 'Int'@ 在内存中的表示是不同的：

@
-- PrimArray 直接存放原始类型的值
fromList [1,2..] :: PrimArray Int   
+------------------+-------------+-------------+-----
+ PrimArray header | 0x00000000# | 0x00000001# | ...
+------------------+-------------+-------------+-----

-- Array 存放盒装类型的指针
fromList [1,2..] :: Array Int 
+--------------+-----+-----+-----
+ Array header |  *  |  *  | ...
+--------------+--+--+--+--+-----
                  |  |   +------------+-------------+
                  |  +-->+ Int header | 0x00000001# |
                  V      +------------+-------------+
          +-------+----+-------------+
          | Int header | 0x00000000# |
          +------------+-------------+
@

基于上述情况，本模块的主要作用就是提供一个 'Arrays' 类型类来简化各种不同类型的数组操作。例如你可以使用本模块提供的 'readMutableArray' 来读取不同的可变数组类型而不必担心类型不匹配。 关于数组类型的选择有以下考虑:

* 如果是保存原始类型( 'Prim' 类型类的实例)，请使用 'PrimArray' 来减少不必要的跳转和内存消耗。
* 如果是保存 'Data.IORef.IORef', 'Control.Concurrent.MVar.MVar' 或者数组这类本身已经盒装的类型（他们的构造函数只是增加了一层盒装），
  请使用 'UnliftedArray' (或者 'UnliftedSmallArray'，参考下面讨论)。
* 如果是保存大部分 @data@ 定义出来的盒装类型，请使用 'A.Array'，但请注意它的对应可变类型 'A.MutableArray' 会使用
  card-marking 的技巧来加速长时间驻留内存的 GC 效率。如果你的数组长度不大、或者不需要长时间以可变形态驻留内存，
  那么可以考虑使用 'SmallArray'('SmallMutableArray').

另外 GHC 提供的数组操作默认都是不经过边界检查的，如果遇到需要检查程序越界访问的情况，
请打开 GHC 的编译选项 [check-prim-bounds](https://downloads.haskell.org/ghc/latest/docs/users_guide/debugging.html#ghc-flag--fcheck-prim-bounds)。

参考资料：

* [Array heap object layout](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#arrays)
* [Improve array GC with card table](https://gitlab.haskell.org/ghc/ghc/-/issues/650)
* [Add SmallArray# type with no card table](https://gitlab.haskell.org/ghc/ghc/-/issues/8923)
-}
#else
{-|
Module      : Z.Data.Array
Description : Unified boxed and unboxed arrays
Copyright   : (c) Dong Han, 2023
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Haskell's array types are not quite the same as those provided by most languages:

* There is no syntax level support in Haskell, for examples operations like @array[3]@ have to be done with @indexArray array 3@ in Haskell
* Haskell distinguishes between mutable and immutable arrays, and operations on mutable arrays must be done in the 'ST', 'IO' or other 'PrimMonad', so please ensure that you have a good understanding of the 'ST', 'IO' monad before using the functions in this module.
* GHC distinguishes between array containers with different types of elements, for examples @'PrimArray' 'Int'@ and @'Array' 'Int'@ are represented differently in memory:

@
-- PrimArray holds the value of the primitive type directly
fromList [1,2..] :: PrimArray Int   
+------------------+-------------+-------------+-----
+ PrimArray header | 0x00000000# | 0x00000001# | ...
+------------------+-------------+-------------+-----

-- Array store pointers to the boxed type boxes
fromList [1,2..] :: Array Int 
+--------------+-----+-----+-----
+ Array header |  *  |  *  | ...
+--------------+--+--+--+--+-----
                  |  |   +------------+-------------+
                  |  +-->+ Int header | 0x00000001# |
                  V      +------------+-------------+
          +-------+----+-------------+
          | Int header | 0x00000000# |
          +------------+-------------+
@

Based on the above, the main role of this module is to provide the 'Arrays' type class to unify various array operations. For example, you can use the 'readMutableArray' provided by this module to read different mutable array types without having to worry about type mismatches. The following considerations apply to the choice of concrete array type.

* If you are saving primitive types (instances of the 'Prim' type class), use 'PrimArray' to reduce unnecessary jumps and memory consumption.
* If you are saving types such as 'IORef', 'MVar' or arrays that are themselves already boxed on the heap (their constructors just add a layer of boxing), please
  use 'UnliftedArray' (or 'UnliftedSmallArray', see the discussion below).
* For boxed types that hold most of the data type defined via @data@, use 'A.Array', but note that its mutable type, 'A.MutableArray', uses the
  card-marking trick to speed up the GC efficiency. If your arrays are not very large, or do not need to remain in memory in mutable form for long periods of time, then consider using 'SmallArray' ('SmallMutableArray').

In addition, the array operations provided by GHC are not bound checked by default.
Please turn on the GHC compile option [check-prim-bounds](https://downloads.haskell.org/ghc/latest/docs/users_guide/debugging.html#ghc-flag--fcheck-prim- bounds).

Reference:

* [Array heap object layout](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#arrays)
* [Improve array GC with card table](https://gitlab.haskell.org/ghc/ghc/-/issues/650)
* [Add SmallArray# type with no card table](https://gitlab.haskell.org/ghc/ghc/-/issues/8923)
-}
#endif


module Z.Data.Array (
  -- * Arrays Typeclass
    Arrays(..)
  , singletonArray, doubletonArray, tripletonArray, sliceArray
  , modifyAtArray, insertAtArray, deleteAtArray, swapArray
  , swapMutableArray, growMutableArray, shuffleMutableArray
  , RealWorld
  -- * Array & SmallArray Types
  , A.Array(..)
  , A.MutableArray(..)
  , SmallArray(..)
  , SmallMutableArray(..)
  , ArrayException(..)
  , uninitialized
  -- * UnliftedArray & UnliftedSmallArray Types
  , UnliftedArray(..)
  , MutableUnliftedArray(..)
  , UnliftedSmallArray(..)
  , MutableUnliftedSmallArray(..)
  , PrimUnlifted(..)
  -- * PrimArray Types
  , PrimArray(..)
  , MutablePrimArray(..)
  , newPinnedPrimArray, newAlignedPinnedPrimArray
  , copyPrimArrayToPtr, copyMutablePrimArrayToPtr, copyPtrToMutablePrimArray
  , primArrayContents, mutablePrimArrayContents
  , withPrimArrayContents, withMutablePrimArrayContents
  , isPrimArrayPinned, isMutablePrimArrayPinned
  , sizeOf
  , Prim(..)
  ) where

import           Control.Exception              (ArrayException (..), throw)
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits                      (unsafeShiftL)
import           Data.Kind                      (Type)
import qualified Data.Primitive.Array           as A
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Primitive.SmallArray
import           Data.Primitive.Types
import           GHC.Exts
import           System.Random.Stateful         ( UniformRange(uniformRM), StatefulGen )
import           Z.Data.Array.UnliftedArray
import           Z.Data.Array.UnliftedSmallArray
import           Z.Data.Utils.Orphan.Prim            ()


#ifdef CN_DOC
-- | 使用 'newArray' 创建盒装数组 'Array' 的默认填充值。
-- 如果该默认值被使用会触发报错 'UndefinedElement'。
#else
-- | Default placeholder for creating boxed array using 'newArray', 
-- if this value is ever used, throw 'UndefinedElement'.
#endif
uninitialized :: a
uninitialized = throw (UndefinedElement "Data.Array.uninitialized")


#ifdef CN_DOC
-- | 提供统一数组操作的数组类型类。
-- 大部分操作的语义对所有数组实例类型来说都是一致，但这里有一个例外：
-- 'shrinkMutableArray' 对于 'A.MutableArray' 和 'MutableUnliftedArray' 来说是无效的，即
-- 数组的长度在 shrink 前后不会发生改变（这涉及到他们的内存布局和 card table），对于其他的
-- 数组类型来说，'shrinkMutableArray' 会改变数组长度。
#else
-- | Type class that provides uniform array operations.
-- The semantics of most operations are consistent for all array instance types, with one exception here:
-- 'shrinkMutableArray' will do nothing on 'A.MutableArray' and 'A.MutableUnliftedArray', i.e.
-- the length of the array does not change before and after shrink
-- (this relates to their memory layout and card table), for other
-- array types, 'shrinkMutableArray' will change the length of the array.
#endif
class Arrays (arr :: Type -> Type) a where

#ifdef CN_DOC
    -- | arr 数组对应的可变数组的类型，例如 @'Mutable' 'A.Array' = 'A.MutableArray'@。
    -- 注意可变数组需要在 'PrimMonad' 里操作，第一个类型参数是 'PrimMonad' 的 state token 类型。 
    -- 例如 @'A.MutableArray' 'RealWorld' Foo@ 是一个可以在 'IO' 中使用的可变数组。
#else
    -- | The mutable version of arr type.
    -- The mutable array must be accessed within a 'PrimMonad', the type reflect that
    -- by requiring the state token type of 'PrimMonad'.
#endif
    type Mutable arr = (mar :: Type -> Type -> Type) | mar -> arr

#ifdef CN_DOC
    -- | 创建指定长度的可变数组
    -- 对于盒装的数组（'A.Array', 'SmallArray'）来说默认填充元素是 'uninitialized'，
    -- 对于原始类型数组（'PrimArray'）来说默认元素是随机的。
#else
    -- | Make a new array with a given size.
    -- For boxed arrays, all elements are 'uninitialized' , which shall not be accessed.
    -- For primitive arrays, elements are random.
#endif
    newMutableArray :: PrimMonad m => Int -> m (Mutable arr (PrimState m) a)

#ifdef CN_DOC
    -- | 创建指定长度的可变数组并填充初始值
#else
    -- | Make a new array and fill it with an initial value.
#endif
    newMutableArrayWith :: PrimMonad m => Int -> a -> m (Mutable arr (PrimState m) a)

#ifdef CN_DOC
    -- | 读取指定下标的值
#else
    -- | Read from specified index of mutable array in a primitive monad.
#endif
    readMutableArray :: PrimMonad m => Mutable arr (PrimState m) a -> Int -> m a

#ifdef CN_DOC
    -- | 把值写入指定下标。
#else
    -- | Write to specified index of mutable array in a primitive monad.
#endif
    writeMutableArray :: PrimMonad m => Mutable arr (PrimState m) a -> Int -> a -> m ()

#ifdef CN_DOC
    -- | 填充可变数组的指定范围。
#else
    -- | Fill the mutable array with a given value.
#endif
    setMutableArray 
        :: PrimMonad m 
        => Mutable arr (PrimState m) a
        -> Int  -- ^ start offset
        -> Int  -- ^ length
        -> a    -- ^ value to be filled
        -> m ()

#ifdef CN_DOC
    -- | 把可变数组的指定范围拷贝创建为一个不可变数组。
#else
    -- | Create an immutable copy of a slice of an array.
#endif
    freezeMutableArray 
        :: PrimMonad m
        => Mutable arr (PrimState m) a
        -> Int      -- ^ offset
        -> Int      -- ^ length
        -> m (arr a)

#ifdef CN_DOC
    -- | 把可变数组创建为一个不可变数组（无拷贝）。
    -- 由于没用拷贝发生，该操作后可变数组不应该再被使用。
#else
    -- | Convert a mutable array to an immutable one without copying.
    -- The array should not be modified after the conversion.
#endif
    unsafeFreezeMutableArray :: PrimMonad m => Mutable arr (PrimState m) a -> m (arr a)

#ifdef CN_DOC
    -- | 把可变数组的指定范围拷贝到另一个可变数组（数组应该不同）。
#else
    -- | Copy a slice of a mutable array to another mutable array at given offset.
    -- The two mutable arrays should not be the same.
#endif
    copyMutableArray
        :: PrimMonad m 
        => Mutable arr (PrimState m) a  -- ^ target
        -> Int           -- ^ offset into target array
        -> Mutable arr (PrimState m) a  -- ^ source
        -> Int           -- ^ offset into source array
        -> Int           -- ^ number of elements to copy
        -> m ()

#ifdef CN_DOC
    -- | 把可变数组的指定范围拷贝到另一个可变数组（数组可以相同）。
#else
    -- | Copy a slice of a mutable array to a mutable array at given offset.
    -- The two mutable arrays can be the same.
#endif
    moveMutableArray
        :: PrimMonad m 
        => Mutable arr (PrimState m) a  -- ^ target
        -> Int           -- ^ offset into target array
        -> Mutable arr (PrimState m) a  -- ^ source
        -> Int           -- ^ offset into source array
        -> Int           -- ^ number of elements to copy
        -> m ()

#ifdef CN_DOC
    -- | 把可变数组的指定范围拷贝到一个新的可变数组。
#else
    -- | Create a mutable copy the given subrange of the original array.
#endif
    cloneMutableArray
        :: PrimMonad m
        => Mutable arr (PrimState m) a 
        -> Int  -- ^ offset
        -> Int  -- ^ length
        -> m (Mutable arr (PrimState m) a)


#ifdef CN_DOC
    -- | 扩大可变数组到新的长度（目前的实现总会创建新的数组）。
#else
    -- | Resize a mutable array to the given size (always a new array in current implementation).
#endif
    resizeMutableArray
        :: PrimMonad m 
        => Mutable arr (PrimState m) a
        -> Int      -- ^ new size
        -> m (Mutable arr (PrimState m) a)

#ifdef CN_DOC
    -- | 缩小可变数组到新的长度，该操作对'A.MutableArray' 和 'UnliftedMutableArray' 无效。
#else
    -- | Shrink a mutable array to the given size. This operation DOES NOT works on 'A.MutableArray', 
    -- and 'UnliftedMutableArray', i.e. 'sizeOfMutableArr' will not change.
#endif
    shrinkMutableArray :: PrimMonad m => Mutable arr (PrimState m) a -> Int -> m ()

#ifdef CN_DOC
    -- | 判断两个可变数组是否指向同一个对象。 
#else
    -- | Is two mutable array are reference equal.
#endif
    sameMutableArray :: Mutable arr s a -> Mutable arr s a -> Bool

#ifdef CN_DOC
    -- | 获取可变数组的长度（元素个数）。
#else
    -- | Size of the mutable array.
#endif
    sizeofMutableArray :: PrimMonad m => Mutable arr (PrimState m) a -> m Int

--------------------------------------------------------------------------------

#ifdef CN_DOC
    -- | 空数组。
#else
    -- | The empty array reference.
#endif
    emptyArray :: arr a

#ifdef CN_DOC
    -- | 读取数组元素，如不及时 force 求值可能会生成一个取值的任务盒，
    -- 参考下面 'indexArrayM' 的讨论。
#else
    -- | Read from the specified index of an immutable array. It's pure and often
    -- results in an indexing thunk, use 'indexArrayM' to avoid this.
#endif
    indexArray :: arr a -> Int -> a

#ifdef CN_DOC
    -- | 读取数组元素，并把结果放在一个 unboxed tuple 里面，当对 tuple 进行模式匹配时，读取动作会发生，
    -- 并且不会进一步对读取之后的值进行求值。该函数在某些需要保留数组惰性行为的地方有用。
#else
    -- | Read from the specified index of an immutable array. The result is packaged into an unboxed unary tuple;
    -- the result itself is not yet evaluated.
    -- Pattern matching on the tuple forces the indexing of the array to happen but does not evaluate the element itself.
    -- Evaluating the thunk prevents additional thunks from building up on the heap.
    -- Avoiding these thunks, in turn, reduces references to the argument array, allowing it to be garbage collected more promptly.
#endif
    indexArray' :: arr a -> Int -> (# a #)

#ifdef CN_DOC
    -- | 在 monad 里读取数组元素，这个函数可以更方便地实现 'indexArray'' 的功能，
    -- 而不需要匹配 unboxed tuple，如下所示：
    --
    -- > -- 这样会导致往 j 位置写入一个任务盒
    -- > foo marr arr ... = do ...
    -- >                       let x = indexArray arr i
    -- >                       writeArray arr j x
    -- > -- 这样则不会
    -- > foo marr arr ... = do ...
    -- >                       x <- indexArrayM arr i
    -- >                       writeArray arr j x
    --
    -- 这里具体的原理是 '>>=' 会模式匹配 @indexArray' arr i of (# x #) -> pure x@
    -- 来获得 @x@，而 @(# x #)@ 作为 unlifted type，会先于 pure x 求值。
#else
    -- | Monadically read a value from the immutable array at the given index. 
    -- This is more convenient to write than 'indexArray'' sometime.
    --
    -- > -- This will write a thunk into j
    -- > foo marr arr ... = do ...
    -- >                       let x = indexArray arr i
    -- >                       writeArray arr j x
    -- > -- This will not
    -- > foo marr arr ... = do ...
    -- >                       x <- indexArrayM arr i
    -- >                       writeArray arr j x
    --
    -- This is done by '>>=' trying to pattern match on @indexArray' arr i of (# x #) -> pure x@
    -- while @(# x #)@ is an unlifted type, which get evaluate before the @pure x@ being matched.
#endif
    indexArrayM :: (Monad m) => arr a -> Int -> m a

#ifdef CN_DOC
    -- | 把数组的指定范围拷贝到一个新的可变数组以便后续操作。
#else
    -- | Create a mutable array from a slice of an immutable array.
    -- This operation makes a copy of the specified slice, so it is safe to use the immutable array afterward.
#endif
    thawArray :: PrimMonad m => arr a -> Int -> Int -> m (Mutable arr (PrimState m) a)

#ifdef CN_DOC
    -- | 把数组转为可变数组以便后续操作。因为没有拷贝发生，后续更改后之前的不可变数组
    -- 不可以再被使用。
#else
    -- | Convert a mutable array to an immutable one without copying. The
    -- array should not be modified after the conversion.
#endif
    unsafeThawArray :: PrimMonad m => arr a -> m (Mutable arr (PrimState m) a)

#ifdef CN_DOC
    -- | 把数组的指定范围拷贝到另一个可变数组的指定位置。
#else
    -- | Copy a slice of an immutable array to a mutable array at given offset.
#endif
    copyArray :: PrimMonad m 
              => Mutable arr (PrimState m) a -- ^ target
              -> Int          -- ^ offset into target array
              -> arr a        -- ^ source array
              -> Int          -- ^ offset into source array
              -> Int          -- ^ number of elements to copy
              -> m ()

#ifdef CN_DOC
    -- | 把数组的指定范围创建为另一个新的数组。
#else
    -- | Create an immutable copy with the given subrange of the original array.
#endif
    cloneArray :: arr a -> Int -> Int -> arr a

#ifdef CN_DOC
    -- | 获取数组长度。
#else
    -- | Size of the immutable array.
#endif
    sizeofArray :: arr a -> Int

#ifdef CN_DOC
    -- | 检查数组是否指向相同的对象。
    --
    -- 注意，该函数的结果可能会随着编译器优化的不同而不同，例如
    -- @let arr = runST ... in arr \`sameArr\` arr@ 可能会返回 'False' 如果编译器决定内联 @arr@ 两次。
#else
    -- | Check whether the two immutable arrays refer to the same memory block
    --
    -- Note that the result of 'sameArr' may change depending on compiler's optimizations, for example,
    -- @let arr = runST ... in arr \`sameArr\` arr@ may return 'False' if compiler decides to
    -- inline it.
#endif
    sameArray :: arr a -> arr a -> Bool

instance Arrays A.Array a where
    type Mutable A.Array = A.MutableArray
    emptyArray = A.emptyArray
    {-# INLINE emptyArray #-}
    newMutableArray n = A.newArray n uninitialized
    {-# INLINE newMutableArray #-}
    newMutableArrayWith = A.newArray
    {-# INLINE newMutableArrayWith #-}
    readMutableArray = A.readArray
    {-# INLINE readMutableArray #-}
    writeMutableArray = A.writeArray
    {-# INLINE writeMutableArray #-}
    setMutableArray marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = A.writeArray marr i x >> go (i+1)
    {-# INLINE setMutableArray #-}
    indexArray = A.indexArray
    {-# INLINE indexArray #-}
    indexArray' (A.Array arr#) (I# i#) = indexArray# arr# i#
    {-# INLINE indexArray' #-}
    indexArrayM = A.indexArrayM
    {-# INLINE indexArrayM #-}
    freezeMutableArray = A.freezeArray
    {-# INLINE freezeMutableArray #-}
    thawArray = A.thawArray
    {-# INLINE thawArray #-}
    unsafeFreezeMutableArray = A.unsafeFreezeArray
    {-# INLINE unsafeFreezeMutableArray #-}
    unsafeThawArray = A.unsafeThawArray
    {-# INLINE unsafeThawArray #-}

    copyArray = A.copyArray
    {-# INLINE copyArray #-}
    copyMutableArray = A.copyMutableArray
    {-# INLINE copyMutableArray #-}

    -- copyArray# checked for overlapped situation
    moveMutableArray = A.copyMutableArray
    {-# INLINE moveMutableArray #-}

    cloneArray = A.cloneArray
    {-# INLINE cloneArray #-}
    cloneMutableArray = A.cloneMutableArray
    {-# INLINE cloneMutableArray #-}

    resizeMutableArray marr n = do
        marr' <- A.newArray n uninitialized
        A.copyMutableArray marr' 0 marr 0 (A.sizeofMutableArray marr)
        return marr'
    {-# INLINE resizeMutableArray #-}
    shrinkMutableArray _ _ = return ()
    {-# INLINE shrinkMutableArray #-}

    sameMutableArray = A.sameMutableArray
    {-# INLINE sameMutableArray #-}
    sizeofArray = A.sizeofArray
    {-# INLINE sizeofArray #-}
    sizeofMutableArray = return . A.sizeofMutableArray
    {-# INLINE sizeofMutableArray #-}

    sameArray (A.Array arr1#) (A.Array arr2#) = isTrue# (
        sameMutableArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
    {-# INLINE sameArray #-}

instance Arrays SmallArray a where
    type Mutable SmallArray = SmallMutableArray
    emptyArray = emptySmallArray
    {-# INLINE emptyArray #-}
    newMutableArray n = newSmallArray n uninitialized
    {-# INLINE newMutableArray #-}
    newMutableArrayWith = newSmallArray
    {-# INLINE newMutableArrayWith #-}
    readMutableArray = readSmallArray
    {-# INLINE readMutableArray #-}
    writeMutableArray = writeSmallArray
    {-# INLINE writeMutableArray #-}
    setMutableArray marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeSmallArray marr i x >> go (i+1)
    {-# INLINE setMutableArray #-}
    indexArray = indexSmallArray
    {-# INLINE indexArray #-}
    indexArray' (SmallArray arr#) (I# i#) = indexSmallArray# arr# i#
    {-# INLINE indexArray' #-}
    indexArrayM = indexSmallArrayM
    {-# INLINE indexArrayM #-}
    freezeMutableArray = freezeSmallArray
    {-# INLINE freezeMutableArray #-}
    thawArray = thawSmallArray
    {-# INLINE thawArray #-}
    unsafeFreezeMutableArray = unsafeFreezeSmallArray
    {-# INLINE unsafeFreezeMutableArray #-}
    unsafeThawArray = unsafeThawSmallArray
    {-# INLINE unsafeThawArray #-}

    copyArray = copySmallArray
    {-# INLINE copyArray #-}
    copyMutableArray = copySmallMutableArray
    {-# INLINE copyMutableArray #-}
    
    -- copyArray# checked for overlapped situation
    moveMutableArray = copySmallMutableArray
    {-# INLINE moveMutableArray #-}

    cloneArray = cloneSmallArray
    {-# INLINE cloneArray #-}
    cloneMutableArray = cloneSmallMutableArray
    {-# INLINE cloneMutableArray #-}

    resizeMutableArray marr n = do
        marr' <- newSmallArray n uninitialized
        copySmallMutableArray marr' 0 marr 0 (sizeofSmallMutableArray marr)
        return marr'
    {-# INLINE resizeMutableArray #-}
    shrinkMutableArray = shrinkSmallMutableArray
    {-# INLINE shrinkMutableArray #-}

    sameMutableArray (SmallMutableArray smarr1#) (SmallMutableArray smarr2#) =
        isTrue# (sameSmallMutableArray# smarr1# smarr2#)
    {-# INLINE sameMutableArray #-}
    sizeofArray = sizeofSmallArray
    {-# INLINE sizeofArray #-}
    sizeofMutableArray = return . sizeofSmallMutableArray
    {-# INLINE sizeofMutableArray #-}

    sameArray (SmallArray arr1#) (SmallArray arr2#) = isTrue# (
        sameSmallMutableArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
    {-# INLINE sameArray #-}

instance Prim a => Arrays PrimArray a where
    type Mutable PrimArray = MutablePrimArray
    emptyArray = emptyPrimArray
    {-# INLINE emptyArray #-}
    newMutableArray = newPrimArray
    {-# INLINE newMutableArray #-}
    newMutableArrayWith n x = do
        marr <- newPrimArray n
        when (n > 0) (setPrimArray marr 0 n x)
        return marr
    {-# INLINE newMutableArrayWith #-}
    readMutableArray = readPrimArray
    {-# INLINE readMutableArray #-}
    writeMutableArray = writePrimArray
    {-# INLINE writeMutableArray #-}
    setMutableArray = setPrimArray
    {-# INLINE setMutableArray #-}
    indexArray = indexPrimArray
    {-# INLINE indexArray #-}
    indexArray' arr i = case indexPrimArray arr i of !x -> (# x #)
    {-# INLINE indexArray' #-}
    indexArrayM arr i = return $! indexPrimArray arr i
    {-# INLINE indexArrayM #-}
    freezeMutableArray = freezePrimArray
    {-# INLINE freezeMutableArray #-}
    thawArray = thawPrimArray
    {-# INLINE thawArray #-}
    unsafeFreezeMutableArray = unsafeFreezePrimArray
    {-# INLINE unsafeFreezeMutableArray #-}
    unsafeThawArray = unsafeThawPrimArray
    {-# INLINE unsafeThawArray #-}

    copyArray = copyPrimArray
    {-# INLINE copyArray #-}
    copyMutableArray = copyMutablePrimArray
    {-# INLINE copyMutableArray #-}

    moveMutableArray (MutablePrimArray dst) doff (MutablePrimArray src) soff n =
        moveByteArray (MutableByteArray dst) (doff*siz) (MutableByteArray src) (soff*siz) (n*siz)
      where siz = sizeOf (undefined :: a)
    {-# INLINE moveMutableArray #-}

    cloneArray = clonePrimArray
    {-# INLINE cloneArray #-}
    cloneMutableArray = cloneMutablePrimArray
    {-# INLINE cloneMutableArray #-}

    resizeMutableArray = resizeMutablePrimArray
    {-# INLINE resizeMutableArray #-}
    shrinkMutableArray = shrinkMutablePrimArray
    {-# INLINE shrinkMutableArray #-}

    sameMutableArray = sameMutablePrimArray
    {-# INLINE sameMutableArray #-}
    sizeofArray = sizeofPrimArray
    {-# INLINE sizeofArray #-}
    sizeofMutableArray = getSizeofMutablePrimArray
    {-# INLINE sizeofMutableArray #-}

    sameArray (PrimArray ba1#) (PrimArray ba2#) =
        isTrue# (sameMutableByteArray# (unsafeCoerce# ba1#) (unsafeCoerce# ba2#))
    {-# INLINE sameArray #-}

instance PrimUnlifted a => Arrays UnliftedArray a where
    type Mutable UnliftedArray = MutableUnliftedArray
    emptyArray = emptyUnliftedArray
    {-# INLINE emptyArray #-}
    newMutableArray = unsafeNewUnliftedArray
    {-# INLINE newMutableArray #-}
    newMutableArrayWith = newUnliftedArray
    {-# INLINE newMutableArrayWith #-}
    readMutableArray = readUnliftedArray
    {-# INLINE readMutableArray #-}
    writeMutableArray = writeUnliftedArray
    {-# INLINE writeMutableArray #-}
    setMutableArray = setUnliftedArray
    {-# INLINE setMutableArray #-}
    indexArray = indexUnliftedArray
    {-# INLINE indexArray #-}
    indexArray' = indexUnliftedArray'
    {-# INLINE indexArray' #-}
    indexArrayM = indexUnliftedArrayM
    {-# INLINE indexArrayM #-}
    freezeMutableArray = freezeUnliftedArray
    {-# INLINE freezeMutableArray #-}
    thawArray = thawUnliftedArray
    {-# INLINE thawArray #-}
    unsafeFreezeMutableArray = unsafeFreezeUnliftedArray
    {-# INLINE unsafeFreezeMutableArray #-}
    unsafeThawArray = unsafeThawUnliftedArray
    {-# INLINE unsafeThawArray #-}

    copyArray = copyUnliftedArray
    {-# INLINE copyArray #-}
    copyMutableArray = copyMutableUnliftedArray
    {-# INLINE copyMutableArray #-}

    -- copyArray# checked for overlapped situation
    moveMutableArray = copyMutableUnliftedArray
    {-# INLINE moveMutableArray #-}

    cloneArray = cloneUnliftedArray
    {-# INLINE cloneArray #-}
    cloneMutableArray = cloneMutableUnliftedArray
    {-# INLINE cloneMutableArray #-}

    resizeMutableArray marr n = do
        marr' <- newUnliftedArray n uninitialized
        copyMutableUnliftedArray marr' 0 marr 0 (sizeofMutableUnliftedArray marr)
        return marr'
    {-# INLINE resizeMutableArray #-}
    shrinkMutableArray _ _ = return ()
    {-# INLINE shrinkMutableArray #-}

    sameMutableArray = sameMutableUnliftedArray
    {-# INLINE sameMutableArray #-}
    sizeofArray = sizeofUnliftedArray
    {-# INLINE sizeofArray #-}
    sizeofMutableArray = return . sizeofMutableUnliftedArray
    {-# INLINE sizeofMutableArray #-}

    sameArray (UnliftedArray arr1#) (UnliftedArray arr2#) = isTrue# (
        sameMutableArrayArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
    {-# INLINE sameArray #-}

instance PrimUnlifted a => Arrays UnliftedSmallArray a where
    type Mutable UnliftedSmallArray = MutableUnliftedSmallArray
    emptyArray = emptyUnliftedSmallArray
    {-# INLINE emptyArray #-}
    newMutableArray = unsafeNewUnliftedSmallArray
    {-# INLINE newMutableArray #-}
    newMutableArrayWith = newUnliftedSmallArray
    {-# INLINE newMutableArrayWith #-}
    readMutableArray = readUnliftedSmallArray
    {-# INLINE readMutableArray #-}
    writeMutableArray = writeUnliftedSmallArray
    {-# INLINE writeMutableArray #-}
    setMutableArray = setUnliftedSmallArray
    {-# INLINE setMutableArray #-}
    indexArray = indexUnliftedSmallArray
    {-# INLINE indexArray #-}
    indexArray' = indexUnliftedSmallArray'
    {-# INLINE indexArray' #-}
    indexArrayM = indexUnliftedSmallArrayM
    {-# INLINE indexArrayM #-}
    freezeMutableArray = freezeUnliftedSmallArray
    {-# INLINE freezeMutableArray #-}
    thawArray = thawUnliftedSmallArray
    {-# INLINE thawArray #-}
    unsafeFreezeMutableArray = unsafeFreezeUnliftedSmallArray
    {-# INLINE unsafeFreezeMutableArray #-}
    unsafeThawArray = unsafeThawUnliftedSmallArray
    {-# INLINE unsafeThawArray #-}

    copyArray = copyUnliftedSmallArray
    {-# INLINE copyArray #-}
    copyMutableArray = copyMutableUnliftedSmallArray
    {-# INLINE copyMutableArray #-}

    -- copyArray# checked for overlapped situation
    moveMutableArray = copyMutableUnliftedSmallArray
    {-# INLINE moveMutableArray #-}

    cloneArray = cloneUnliftedSmallArray
    {-# INLINE cloneArray #-}
    cloneMutableArray = cloneMutableUnliftedSmallArray
    {-# INLINE cloneMutableArray #-}

    resizeMutableArray marr n = do
        marr' <- newUnliftedSmallArray n uninitialized
        copyMutableUnliftedSmallArray marr' 0 marr 0 (sizeofMutableUnliftedSmallArray marr)
        return marr'
    {-# INLINE resizeMutableArray #-}
    shrinkMutableArray = shrinkMutableUnliftedSmallArray
    {-# INLINE shrinkMutableArray #-}

    sameMutableArray = sameMutableUnliftedSmallArray
    {-# INLINE sameMutableArray #-}
    sizeofArray = sizeofUnliftedSmallArray
    {-# INLINE sizeofArray #-}
    sizeofMutableArray = return . sizeofMutableUnliftedSmallArray
    {-# INLINE sizeofMutableArray #-}

    sameArray (UnliftedSmallArray arr1#) (UnliftedSmallArray arr2#) = isTrue# (
        sameMutableArrayArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
    {-# INLINE sameArray #-}

--------------------------------------------------------------------------------

#ifdef CN_DOC
-- | 拿到 'PrimArray' 的指针并执行操作，执行期间该指针始终有效，但不保证不被 GC 移动。
-- 不要给 'withPrimArrayContents' 传递一个可能会发散的计算，
-- 参考 <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>。
#else
-- | Obtain the pointer to the content of an array, and the pointer should only be used during the IO action.
-- This operation is only safe on /pinned/ primitive arrays (Arrays allocated by 'newPinnedPrimArray' or
-- 'newAlignedPinnedPrimArray').
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
#endif
withPrimArrayContents :: PrimArray a -> (Ptr a -> IO b) -> IO b
{-# INLINE withPrimArrayContents #-}
withPrimArrayContents (PrimArray ba#) f = do
    let addr# = byteArrayContents# ba#
        ptr = Ptr addr#
    b <- f ptr
    primitive_ (touch# ba#)
    return b

-- | Obtain the pointer to the content of an mutable array, and the pointer should only be used during the IO action.
--
-- This operation is only safe on /pinned/ primitive arrays (Arrays allocated by 'newPinnedPrimArray' or
-- 'newAlignedPinnedPrimArray').
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withMutablePrimArrayContents :: MutablePrimArray RealWorld a -> (Ptr a -> IO b) -> IO b
{-# INLINE withMutablePrimArrayContents #-}
withMutablePrimArrayContents (MutablePrimArray mba#) f = do
    let addr# = byteArrayContents# (unsafeCoerce# mba#)
        ptr = Ptr addr#
    b <- f ptr
    primitive_ (touch# mba#)
    return b

--------------------------------------------------------------------------------

#ifdef CN_DOC
-- | 创建一个单元素数组
#else
-- | Create a single element array.
#endif
singletonArray :: Arrays arr a => a -> arr a
{-# INLINE singletonArray #-}
singletonArray x = runST $ do
    marr <- newMutableArrayWith 1 x
    unsafeFreezeMutableArray marr

#ifdef CN_DOC
-- | 创建一个双元素数组
#else
-- | Create a double element array.
#endif
doubletonArray :: Arrays arr a => a -> a -> arr a
{-# INLINE doubletonArray #-}
doubletonArray x y = runST $ do
    marr <- newMutableArrayWith 2 x
    writeMutableArray marr 1 y
    unsafeFreezeMutableArray marr

#ifdef CN_DOC
-- | 创建一个三元素数组
#else
-- | Create a triple element array.
#endif
tripletonArray :: Arrays arr a => a -> a -> a -> arr a
{-# INLINE tripletonArray #-}
tripletonArray x y z = runST $ do
    marr <- newMutableArrayWith 3 x
    writeMutableArray marr 1 y
    writeMutableArray marr 2 z
    unsafeFreezeMutableArray marr

#ifdef CN_DOC
-- | 从数组指定范围内创建新数组并插入一个元素。
-- 新的元素会被插入到下标对应的元素前面（如果下标等于给定范围长度，则添加到末尾）。
#else
-- | Insert a value to an immutable array at given index. This function will produce a new array.
#endif
insertAtArray :: Arrays arr a
               => arr a
               -> Int        -- ^ offset
               -> Int        -- ^ length
               -> Int        -- ^ insert index in new array
               -> a          -- ^ value to be inserted
               -> arr a
{-# INLINE insertAtArray #-}
insertAtArray arr s l i x = runST $ do
    marr <- newMutableArray (l+1) 
    when (i>0) $ copyArray marr 0 arr s i
    when (i<l) $ copyArray marr (i+1) arr (i+s) (l-i)
    writeMutableArray marr i x
    unsafeFreezeMutableArray marr

#ifdef CN_DOC
-- | 修改范围内下标对应的元素。
#else
-- | Modify(strictly) an immutable some elements of an array with specified subrange.
-- This function will produce a new array.
#endif
modifyAtArray :: Arrays arr a
               => arr a
               -> Int        -- ^ offset
               -> Int        -- ^ length
               -> Int        -- ^ index in new array
               -> (a -> a)   -- ^ modify function
               -> arr a
{-# INLINE modifyAtArray #-}
modifyAtArray arr off len ix f = runST $ do
    marr <- unsafeThawArray (cloneArray arr off len)
    !v <- f <$> readMutableArray marr ix
    writeMutableArray marr ix v
    unsafeFreezeMutableArray marr


#ifdef CN_DOC
-- | 创建数组指定范围的切片，和'cloneArray'不同的是如果范围是整段数组则直接返回原数组。
#else
-- | Create an immutable slice with the given subrange of the original array, 
-- if the subrange is whole array, return original array without copying .
#endif
sliceArray :: Arrays arr a => arr a -> Int -> Int -> arr a
{-# INLINE sliceArray #-}
sliceArray arr s e | s == 0 && sizeofArray arr == e = arr
                   | otherwise = cloneArray arr s (e-s)

#ifdef CN_DOC
-- | 从数组指定范围内创建新数组并删除一个元素。
#else
-- | Delete an element of the immutable array's at given index. This function will produce a new array.
#endif
deleteAtArray :: Arrays arr a
            => arr a
            -> Int        -- ^ offset
            -> Int        -- ^ length
            -> Int        -- ^ the index of the element to delete
            -> arr a
{-# INLINE deleteAtArray #-}
deleteAtArray arr s l i = runST $ do
    marr <- newMutableArray (l-1)
    when (i>0) $ copyArray marr 0 arr s i
    let i' = i+1
    when (i'<l) $ copyArray marr i arr (i'+s) (l-i')
    unsafeFreezeMutableArray marr

#ifdef CN_DOC
-- | 创建新数组并交换两个下标的值。
#else
-- | Swap two elements under given index and return a new array.
#endif
swapArray :: Arrays arr a
          => arr a
          -> Int
          -> Int
          -> arr a
{-# INLINE swapArray #-}
swapArray arr i j = runST $ do
    marr <- thawArray arr 0 (sizeofArray arr)
    swapMutableArray marr i j
    unsafeFreezeMutableArray marr


#ifdef CN_DOC
-- | 交换可变数组中两个下标的值。
#else
-- | Swap two elements under given index, no atomically guarantee is given.
#endif
swapMutableArray
    :: (PrimMonad m, Arrays arr a)
    => Mutable arr (PrimState m) a
    -> Int
    -> Int
    -> m ()
{-# INLINE swapMutableArray #-}
swapMutableArray marr i j = do
    x <- readMutableArray marr i
    y <- readMutableArray marr j
    writeMutableArray marr i y
    writeMutableArray marr j x

#ifdef CN_DOC
-- | 试图扩大可变数组到指定大小，扩大后的长度为 @max given_size (2 * original_size)@。
#else
-- | Resize mutable array to @max given_size (2 * original_size)@.
#endif
growMutableArray
    :: (PrimMonad m, Arrays arr a) 
    => Mutable arr (PrimState m) a 
    -> Int 
    -> m (Mutable arr (PrimState m) a)
{-# INLINE growMutableArray #-}
growMutableArray marr l = do
    siz <- sizeofMutableArray marr
    if (siz < l)
    then resizeMutableArray marr (max (siz `unsafeShiftL` 1) l)
    else return marr

#ifdef CN_DOC
-- | 使用 <https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle Fisher-Yates> 算法打乱数组。
#else
-- | Shuffle array's elements in slice range use
-- <https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle Fisher-Yates> algorithm.
#endif
shuffleMutableArray 
    :: (PrimMonad m, StatefulGen g m, Arrays arr a)
    => g -> Mutable arr (PrimState m) a
    -> Int  -- ^ offset
    -> Int  -- ^ length
    -> m ()
{-# INLINE shuffleMutableArray #-}
shuffleMutableArray g marr off n = go (off+n-1)
  where
    go i | i < off+1 = return ()
         | otherwise = do
            j <- uniformRM (off, i) g
            swapMutableArray marr i j
            go (i - 1)
