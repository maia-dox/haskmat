module Matrix where

-- a Vector / Matrix Math library for Haskell
-- author : Jim Doxtader


data Vector = Vector1f Float 
              | Vector2f (Float, Float) 
              | Vector3f (Float, Float, Float) 
              | Vector4f (Float, Float, Float, Float)
              deriving(Show, Eq)





-- subtract vector b from vector a
subv :: Vector -> Vector -> Vector
subv (Vector1f v1) (Vector1f v2) = Vector1f (v1 - v2)  
subv (Vector2f (ax,ay)) (Vector2f (bx,by)) = Vector2f ( (ax - bx), (ay - by) )  
subv (Vector3f (ax, ay, az)) (Vector3f (bx, by, bz)) = Vector3f ( (ax - bx), (ay - by), (az - bz) )
subv (Vector4f (ax, ay, az, a4)) (Vector4f(bx, by, bz, b4)) = Vector4f ( (ax - bx), (ay - by), (az - bz), (a4 - b4) )


-- add vector a to vector b
addv :: Vector -> Vector -> Vector
addv (Vector1f v1) (Vector1f v2) = Vector1f (v1 + v2)
addv (Vector2f (ax,ay)) (Vector2f (bx,by)) = Vector2f ( (ax + bx), (ay + by) )
addv (Vector3f (ax, ay, az)) (Vector3f (bx, by, bz)) = Vector3f ( (ax + bx), (ay + by), (az + bz) )
addV (Vector4f (ax, ay, az, a4)) (Vector4f(bx, by, bz, b4)) = Vector4f ( (ax + bx), (ay + by), (az + bz), (a4 + b4) )


-- magnitude of vector
magv :: Vector -> Float
magv (Vector1f v1) = v1
magv (Vector2f (x, y)) = abs ( sqrt( (x^2) + (y^2) ))
magv (Vector3f (x, y, z)) = abs ( sqrt( (x^2) + (y^2) + (z^2) ))
magv (Vector4f (x, y, z, d4) ) = abs ( sqrt( (x^2) + (y^2) + (z^2) + (d4^2) ))



-- direction of vector
dirv :: Vector -> Vector
dirv (Vector1f v) = Vector1f v
dirv (Vector2f (x, y)) = Vector2f( (x / absMag), (y / absMag) ) 
    where 
        absMag =  abs (magv (Vector2f(x, y)) )

dirv (Vector3f (x, y, z)) = Vector3f( (x / absMag), (y / absMag), (z / absMag) )
    where
        absMag = abs (magv (Vector3f(x, y, z)) )

dirv (Vector4f (x, y, z, d4)) = Vector4f( (x / absMag), (y / absMag), (z / absMag), (d4 / absMag) )
    where
        absMag = abs (magv (Vector4f(x, y, z, d4)) )



-- scale vector
scale :: Vector -> Float -> Vector
scale (Vector1f v) m = Vector1f (v * m)
scale (Vector2f (x, y)) m = Vector2f((x * m), (y * m))
scale (Vector3f (x, y, z)) m = Vector3f((x * m), (y * m), (z * m))
scale (Vector4f (x, y, z, d4)) m = Vector4f((x * m), (y * m), (z * m), (d4 * m))



-- dot product 
dot :: Vector -> Vector -> Float
dot (Vector1f v1) (Vector1f v2) = v1 * v2
dot (Vector2f (ax, ay)) (Vector2f (bx, by)) =  (ax * bx) + (ay * by) 
dot (Vector3f (ax, ay, az)) (Vector3f (bx, by, bz)) = (ax * bx) + (ay * by) + (az * bz)  
dot (Vector4f (ax, ay, az, a4)) (Vector4f (bx, by, bz, b4)) =  (ax * bx) + (ay * by) + (az * bz) + (a4 * b4) 




-- cross product
cross :: Vector -> Vector -> Vector
cross (Vector3f (ax, ay, az)) (Vector3f (bx, by, bz)) = Vector3f ( ((ay*bz) - (by*az)), ((az*bx) - (ax*bz)), ((ax*by) - (bx*ay)) ) 





-- _________________________________________________________________


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








