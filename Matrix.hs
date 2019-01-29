module Matrix where
import Vector


data Matrix = Matrix1f [Float]
              | Matrix2f [[Float]]
              | Matrix3f [[[Float]]]
              | Matrix4f [[[[Float]]]]
                deriving(Show, Eq)







-- take in two rows of data from a Matrix, and return the combined row. (Float)
addMatRow :: [Float] -> [Float] -> [Float]
addMatRow [] [] = []
addMatRow (a:matA) (b:matB) = (a + b) : (addMatRow matA matB) 
addMatRow [] matB = matB
addMatRow matA [] = matA


addMatData2f :: [[Float]] -> [[Float]] -> [[Float]]
addMatData2f [] [] = []
addMatData2f (a:matA) (b:matB) = (addMatRow a b) : (addMatData2f matA matB)
addMatData2f [] matB = matB
addMatData2f matA [] = matA


addMatData3f :: [[[Float]]] -> [[[Float]]] -> [[[Float]]]
addMatData3f [] [] = []
addMatData3f (a:matA) (b:matB) = (addMatData2f a b) : (addMatData3f matA matB)
addMatData3f [] matB = matB
addMatData3f matA [] = matA

addMatData4f :: [[[[Float]]]] -> [[[[Float]]]] -> [[[[Float]]]]
addMatData4f [] [] = []
addMatData4f (a:matA) (b:matB) = (addMatData3f a b) : (addMatData4f matA matB)

addmat :: Matrix -> Matrix -> Matrix
addmat (Matrix1f matA) (Matrix1f matB) = Matrix1f (addMatRow matA matB)
addmat (Matrix2f matA) (Matrix2f matB) = Matrix2f (addMatData2f matA matB)
addmat (Matrix3f matA) (Matrix3f matB) = Matrix3f (addMatData3f matA matB)
addmat (Matrix4f matA) (Matrix4f matB) = Matrix4f (addMatData4f matA matB)



subMatRow :: [Float] -> [Float] -> [Float]
subMatRow [] [] = []
subMatRow (a:rowA) (b:rowB) = (a - b) : (subMatRow rowA rowB)
subMatRow [] rowB = rowB
subMatRow rowA [] = rowA

subMatData2f :: [[Float]] -> [[Float]] -> [[Float]]
subMatData2f [] [] = []
subMatData2f (a:matA) (b:matB) = (subMatRow a b) : (addMatData2f matA matB)
subMatData2f [] matB = matB
subMatData2f matA [] = matA

subMatData3f :: [[[Float]]] -> [[[Float]]] -> [[[Float]]]
subMatData3f [] [] = []
subMatData3f (a:matA) (b:matB) = (subMatData2f a b) : (addMatData3f matA matB)
subMatData3f [] matB = matB
subMatData3f matA [] = matA


subMatData4f :: [[[[Float]]]] -> [[[[Float]]]] -> [[[[Float]]]]
subMatData4f [] [] = []
subMatData4f (a:matA) (b:matB) = (subMatData3f a b) : (subMatData4f matA matB)
subMatData4f [] matB = matB
subMatData4f matA [] = matA

submat :: Matrix -> Matrix -> Matrix
submat (Matrix1f matA) (Matrix1f matB) = Matrix1f (subMatRow matA matB)
submat (Matrix2f matA) (Matrix2f matB) = Matrix2f (subMatData2f matA matB)
submat (Matrix3f matA) (Matrix3f matB) = Matrix3f (subMatData3f matA matB)
submat (Matrix4f matA) (Matrix4f matB) = Matrix4f (subMatData4f matA matB)



-- multiplyMat
mulMatRow :: [Float] -> [Float] -> [Float]
mulMatRow [] [] = []
mulMatRow (a:rowA) (b:rowB) = (a * b) : (subMatRow rowA rowB)
mulMatRow [] rowB = rowB
mulMatRow rowA [] = rowA


mulMatData2f :: [[Float]] -> [[Float]] -> [[Float]]
mulMatData2f [] [] = []
mulMatData2f (a:matA) (b:matB) = (mulMatRow a b) : (mulMatData2f matA matB)
mulMatData2f [] matB = matB
mulMatData2f matA [] = matA

mulMatData3f :: [[[Float]]] -> [[[Float]]] -> [[[Float]]]
mulMatData3f [] [] = []
mulMatData3f (a:matA) (b:matB) = (mulMatData2f a b) : (mulMatData3f matA matB)
mulMatData3f [] matB = matB
mulMatData3f matA [] = matA

mulMatData4f :: [[[[Float]]]] -> [[[[Float]]]] -> [[[[Float]]]]
mulMatData4f [] [] = []
mulMatData4f (a:matA) (b:matB) = (mulMatData3f a b) : (mulMatData4f matA matB)
mulMatData4f [] matB = matB
mulMatData4f matA [] = matA

mulMat :: Matrix -> Matrix -> Matrix
mulMat (Matrix1f matA) (Matrix1f matB) = Matrix1f (mulMatRow matA matB)
mulMat (Matrix2f matA) (Matrix2f matB) = Matrix2f (mulMatData2f matA matB)
mulMat (Matrix3f matA) (Matrix3f matB) = Matrix3f (mulMatData3f matA matB)
mulMat (Matrix4f matA) (Matrix4f matB) = Matrix4f (mulMatData4f matA matB)







-- Vector * Matrix operations

-- Convert a Vector into Matrix 
convertVecMat :: Vector -> Matrix
convertVecMat (Vector1f x) = Matrix1f ([x])


addMatVec :: Matrix -> Vector -> Matrix
addMatVec (Matrix1f matA) (Vector1f x) = Matrix1f matA






-- TODO : transformation functions 

