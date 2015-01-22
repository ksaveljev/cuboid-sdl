module Utils where

import FRP.Yampa.Vector3

import Types

p3DtoV3 :: (RealFloat a) => Point3D -> Vector3 a
p3DtoV3 (P3D px py pz) = vector3 (fromInteger px) (fromInteger py) (fromInteger pz)

vectorApply :: (RealFloat a, RealFloat b) => (a -> b) -> Vector3 a -> Vector3 b
vectorApply f v = vector3 (f $ vector3X v) (f $ vector3Y v) (f $ vector3Z v)

vector3Rotate' :: (Integral a, RealFloat b) => a -> Vector3 b -> Vector3 b
vector3Rotate' theta a =
    let rotateTheta 0 v = v                                
        rotateTheta 1 v = vector3 (vector3X v) (vector3Z v)    (-(vector3Y v))
        rotateTheta 2 v = vector3 (vector3X v) (-(vector3Y v)) (-(vector3Z v)) 
        rotateTheta 3 v = vector3 (vector3X v) (-(vector3Z v))   (vector3Y v)
        rotateTheta i v = rotateTheta (abs $ i `mod` 4) v
    in rotateTheta theta a

-- TODO: memoize
size :: Level -> Integer
size = (+1) . maximum . map (\(P3D px py pz) -> maximum [px, py, pz]) . obstacles
