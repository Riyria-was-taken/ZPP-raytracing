{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Utils where

import           Data.Int                      (Int64)
import           System.Random
import           System.Random.Mersenne.Pure64 (PureMT, randomInt64)
import           Text.Printf                   (printf)

--------------------------------------------------------------------------------

data Vec3 = Vec3 { x :: Double, y :: Double, z :: Double } deriving (Eq, Show)

type Point = Vec3
type Color = Vec3

zeroesVec3 :: Vec3
zeroesVec3 = Vec3 { x = 0, y = 0, z = 0 }

idx :: Vec3 -> Integer -> Double
idx v i = case i of
            0 -> v.x
            1 -> v.y
            2 -> v.z

neg :: Vec3 -> Vec3
neg v = Vec3 { x = -v.x, y = -v.y, z = -v.z }

plus :: Vec3 -> Vec3 -> Vec3
plus u v =
    Vec3 { x = u.x + v.x, y = u.y + v.y, z = u.z + v.z }

minus :: Vec3 -> Vec3 -> Vec3
minus u v =
    Vec3 { x = u.x - v.x, y = u.y - v.y, z = u.z - v.z }

times :: Vec3 -> Vec3 -> Vec3
times u v =
    Vec3 { x = u.x * v.x, y = u.y * v.y, z = u.z * v.z }

multiply :: Vec3 -> Double -> Vec3
multiply v t = Vec3 { x = v.x * t, y = v.y * t, z = v.z * t }

divide :: Vec3 -> Double -> Vec3
divide v t = Vec3 { x = v.x / t, y = v.y / t, z = v.z / t }

infixr 6 .+, .-
(.+) :: Vec3 -> Vec3 -> Vec3
u .+ v = plus u v

(.-) :: Vec3 -> Vec3 -> Vec3
u .- v = minus u v

infixr 7 .**
(.**) :: Vec3 -> Vec3 -> Vec3
u .** v = times u v

infixr 7 .*, ./
(.*) :: Vec3 -> Double -> Vec3
v .* t = multiply v t

(./) :: Vec3 -> Double -> Vec3
v ./ t = divide v t

len :: Vec3 -> Double
len v = sqrt $ lenSquared v

lenSquared :: Vec3 -> Double
lenSquared v = dot v v

dot :: Vec3 -> Vec3 -> Double
dot u v = u.x * v.x + u.y * v.y + u.z * v.z

cross :: Vec3 -> Vec3 -> Vec3
cross u v = Vec3 { x = u.y * v.z - u.z + v.y, y = u.z * v.x - u.x * v.z, z = u.x * v.y - u.y * v.x }

unit :: Vec3 -> Vec3
unit v = divide v $ len v

--------------------------------------------------------------------------------

type Interval = (Double, Double)

emptyInterval, universeInterval :: Interval
emptyInterval = (infinity, -infinity)
universeInterval = (-infinity, infinity)

contains :: Interval -> Double -> Bool
contains (min, max) x = min <= x && x <= max

surrounds :: Interval -> Double -> Bool
surrounds (min, max) x = min < x && x < max

clamp :: Interval -> Double -> Double
clamp (min, max) x =
    if x < min then min else if x > max then max else x

--------------------------------------------------------------------------------

data Material = Lambertian { albedo :: Color } | Metal { albedo :: Color, fuzz :: Double } deriving (Eq, Show)

--------------------------------------------------------------------------------

randomDouble :: PureMT -> (Double, PureMT)
randomDouble rng =
    let (d, rng1) = randomInt64 rng in
    (fromIntegral d / (fromIntegral (maxBound :: Int64) + 1.0), rng1)

randomDoubleIn :: PureMT -> Interval -> (Double, PureMT)
randomDoubleIn rng (min, max) =
    let (d, rng1) = randomDouble rng in
    (min + (max - min) * d, rng1)

randomVec3 :: PureMT -> (Vec3, PureMT)
randomVec3 rng =
    let (x, rng1) = randomDouble rng in
    let (y, rng2) = randomDouble rng1 in
    let (z, rng3) = randomDouble rng2 in
    (Vec3 { x = x, y = y, z = z }, rng3)

randomVec3In :: PureMT -> Interval -> (Vec3, PureMT)
randomVec3In rng interval =
    let (x, rng1) = randomDoubleIn rng interval in
    let (y, rng2) = randomDoubleIn rng1 interval in
    let (z, rng3) = randomDoubleIn rng2 interval in
    (Vec3 { x = x, y = y, z = z }, rng3)

randomInUnitSphere :: PureMT -> (Vec3, PureMT)
randomInUnitSphere rng =
    let (v, rng1) = randomVec3In rng (-1, 1) in
    case lenSquared v < 1 of
        True  -> (v, rng1)
        False -> randomInUnitSphere rng1

randomUnitVector :: PureMT -> (Vec3, PureMT)
randomUnitVector rng =
    let (v, rng1) = randomInUnitSphere rng in
    (unit v, rng1)

randomOnHemisphere :: PureMT -> Vec3 -> (Vec3, PureMT)
randomOnHemisphere rng normal =
    let (v, rng1) = randomUnitVector rng in
    case dot v normal > 0.0 of
        True  -> (v, rng1)
        False -> (neg v, rng1)

--------------------------------------------------------------------------------

infinity :: Double
infinity = 1.0 / 0.0

degreesToRadians :: Double -> Double
degreesToRadians deg = deg * pi / 180.0

linearToGamma :: Double -> Double
linearToGamma lin = sqrt lin

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

nearZero :: Vec3 -> Bool
nearZero v =
    let eps = 0.00000001 in
    abs v.x < eps && abs v.y < eps && abs v.z < eps

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v .- n .* (2.0 * dot v n)
