{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Utils where

import           System.IO   (putStrLn)
import           Text.Printf (printf)

data Vec3 = Vec3 { x :: Double, y :: Double, z :: Double } deriving (Eq, Show)

type Point = Vec3
type Color = Vec3

colorSpace :: Integer = 256

zeroesVec3 :: Vec3
zeroesVec3 = Vec3 { x = 0, y = 0, z = 0 }

neg :: Vec3 -> Vec3
neg v = Vec3 { x = -v.x, y = -v.y, z = -v.z }

idx :: Vec3 -> Integer -> Double
idx v i = case i of
            0 -> v.x
            1 -> v.y
            2 -> v.z

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
lenSquared v = v.x * v.x + v.y * v.y + v.z * v.z

dot :: Vec3 -> Vec3 -> Double
dot u v = u.x * v.x + u.y * v.y + u.z * v.z

cross :: Vec3 -> Vec3 -> Vec3
cross u v = Vec3 { x = u.y * v.z - u.z + v.y, y = u.z * v.x - u.x * v.z, z = u.x * v.y - u.y * v.x }

unit :: Vec3 -> Vec3
unit v = divide v $ len v

printPixel :: Color -> IO ()
printPixel color =
    let r :: Integer = round (color.x * fromInteger colorSpace) in
    let g :: Integer = round (color.y * fromInteger colorSpace) in
    let b :: Integer = round (color.z * fromInteger colorSpace) in
    putStrLn $ printf "%d %d %d" r g b
