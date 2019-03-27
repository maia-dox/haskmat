module MathUtils where
import Vector
-- a collection of asorted math functions


-- barycentric coordinates : a + b + y = 1
-- barycentric interpolation function

-- param: position as a vector, (X,Y) coordinates of each vertex of the triangle. (weighted average)
bary_compute :: Vector -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Float
bary_compute vec (x1, y1) (x2, y2) (x3, y3) = w1 + w2 + w3
        where
                calcDist :: Vector -> (Float, Float) -> Float
                calcDist (Vector3f (vx, vy, vz)) (x,y) = sqrt( (x - vx)^2 + (y - vy)^2 )
                w1 = 1 / (calcDist vec (x1,y1) )
                w2 = 1 / (calcDist vec (x2,y2) )
                w3 = 1 / (calcDist vec (x3,y3) )

