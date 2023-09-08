{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Utils where

import           Text.Printf (printf)
import System.Random

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

multiply :: Vec3 -> Double -> Vec3
multiply v t = Vec3 { x = v.x * t, y = v.y * t, z = v.z * t }

divide :: Vec3 -> Double -> Vec3
divide v t = Vec3 { x = v.x / t, y = v.y / t, z = v.z / t }

infixr 6 .+, .-
(.+) :: Vec3 -> Vec3 -> Vec3
u .+ v = plus u v

(.-) :: Vec3 -> Vec3 -> Vec3
u .- v = minus u v

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

randomDouble :: StdGen -> (StdGen, Double)
randomDouble gen =
    let (gen1, d) = randomR (0, 0.999) gen in
    (d, gen1)

randomDoubleIn :: StdGen -> Interval -> (StdGen, Double)
randomDoubleIn gen (min, max) =
    let (newGen, random) = randomDouble gen in
    (newGen, min + (max - min) * random)
    
randomVec3 :: StdGen -> (StdGen, Vec3)
randomVec3 gen =
    let (gen1, randomX) = randomDouble gen in
    let (gen2, randomY) = randomDouble gen1 in
    let (gen3, randomZ) = randomDouble gen2 in
    (gen3, Vec3 { x = randomX, y = randomY, z = randomZ })

randomVec3In :: StdGen -> Interval -> (StdGen, Vec3)
randomVec3In gen interval =
    let (gen1, randomX) = randomDoubleIn gen interval in
    let (gen2, randomY) = randomDoubleIn gen1 interval in
    let (gen3, randomZ) = randomDoubleIn gen2 interval in
    (gen3, Vec3 { x = randomX, y = randomY, z = randomZ })

randomInUnitSphere :: StdGen -> (StdGen, Vec3)
randomInUnitSphere gen =
    let (gen1, v) = randomVec3In gen (-1, 1) in
    case lenSquared v < 1 of
        True -> (gen1, v)
        False -> randomInUnitSphere gen1

randomUnitVector :: StdGen -> (StdGen, Vec3)
randomUnitVector gen = 
    let (gen1, v) = randomInUnitSphere gen in
    (gen1, unit v)

randomOnHemisphere :: StdGen -> Vec3 -> (StdGen, Vec3)
randomOnHemisphere gen normal =
    let (gen1, v) = randomUnitVector gen in
    case dot v normal > 0.0 of
        True -> (gen1, v)
        False -> (gen1, neg v)

--------------------------------------------------------------------------------

infinity :: Double
infinity = 1.0 / 0.0

degreesToRadians :: Double -> Double
degreesToRadians deg = deg * pi / 180.0
    
linearToGamma :: Double -> Double
linearToGamma lin = sqrt lin
