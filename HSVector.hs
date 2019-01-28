module HSVector where



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
