
-- | 2-dimension array slices.
--
class Array (MatrixArray) Matrix mx a
    type MatrixArray mx :: Type -> Type
    fromArray :: Matrix Array a -> Int -> Int -> Int -> Int -> Bool -> mx a
    toArray :: mx a -> (Int, Int, Int, Int, Bool)


data Matrix a = Matrix 
    { matrixArray :: PrimArray a,
      matruxOffset :: Int,
      matrixM :: Int,
      matrixN :: Int,
      matrixStride :: Int
    }


{-
@

Matrix 
                            stride = 11 
            |-------------------------------------------|
                                M = 3
                            |-----------|
            +-------------------------------------------+
            | * | * | * | * | * | * | * | * | * | * | * |
            +-------------------------------------------+             
            | * | * | * | * | * | * | * | * | * | * | * |           
   offset   +---------------+-----------+---------------+ -+
   11*2+3   | * | * | * | * | 1 | 2 | 3 | * | * | * | * |  |   
    =25     +---------------|-----------|---------------+  |  N = 3
            | * | * | * | * | 4 | 5 | 6 | * | * | * | * |  |     
            +---------------|-----------|---------------+ -+
            | * | * | * | * | * | * | * | * | * | * | * |             
            +-------------------------------------------+             
            | * | * | * | * | * | * | * | * | * | * | * |             
            +-------------------------------------------+

@
-}
