{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Camera          (Camera, initCamera, render)
import           Control.DeepSeq
import           Data.List       ((!!))
import           HittableList    (HittableList (..))
import           Sphere          (Sphere (..))
import           System.Environment (getArgs)
import           System.IO       (putStrLn)
import           System.Random
import           Utils           (Material (..), Point, Vec3 (..))

materialGround = Lambertian { albedo = Vec3 { x = 0.7, y = 0.8, z = 0.0 } }
materialCenter = Lambertian { albedo = Vec3 { x = 0.8, y = 0.3, z = 0.3 } }
materialLeft = Metal { albedo = Vec3 { x = 0.8, y = 0.8, z = 0.8 }, fuzz = 0.3 }
materialRight = Metal { albedo = Vec3 { x = 0.8, y = 0.6, z = 0.2 }, fuzz = 1.0 }

worlds :: [HittableList] = [[Sphere { center = Vec3 {x = 0, y = 0, z = -1 }, radius = 0.5, material = materialCenter },
                 Sphere { center = Vec3 {x = 0, y = -100.5, z = -1}, radius = 100, material = materialGround},
                 Sphere { center = Vec3 {x = -1, y = 0, z = -1}, radius = 0.5, material = materialLeft},
                 Sphere { center = Vec3 {x = 1, y = 0, z = -1}, radius = 0.5, material = materialRight}],
                 [Sphere { center = Vec3 {x = 0, y = -100.5, z = -1}, radius = 100, material = materialGround},
                 Sphere { center = Vec3 {x = -2, y = 0, z = -2}, radius = 0.1, material = materialLeft},
                 Sphere { center = Vec3 {x = -1, y = 3, z = -1}, radius = 0.3, material = materialRight},
                 Sphere { center = Vec3 {x = -0.5, y = 0, z = -3}, radius = 0.5, material = materialCenter},
                 Sphere { center = Vec3 {x = -3, y = 1, z = -4}, radius = 1, material = materialGround},
                 Sphere { center = Vec3 {x = 0, y = 2, z = -1}, radius = 2, material = materialRight}],
                [
                 Sphere { center = Vec3 {x = 0, y = -100.5, z = -1}, radius = 100, material = materialGround},
                 Sphere { center = Vec3 {x = -2, y = 0, z = -5}, radius = 0.1, material = materialLeft},
                 Sphere { center = Vec3 {x = -1, y = 3, z = -20}, radius = 5, material = materialRight},
                 Sphere { center = Vec3 {x = -0.5, y = 0, z = -4}, radius = 0.5, material = materialCenter},
                 Sphere { center = Vec3 {x = -3, y = 1, z = -10}, radius = 1, material = materialGround},
                 Sphere { center = Vec3 {x = -2, y = 0, z = -30}, radius = 4, material = materialLeft},
                 Sphere { center = Vec3 {x = -1, y = 3, z = -3}, radius = 0.3, material = materialRight},
                 Sphere { center = Vec3 {x = -4, y = -2, z = -1}, radius = 0.2, material = materialRight}]]


main :: IO ()
main = do
    args <- getArgs
    let world = case args of
                    [] -> worlds !! 0
                    (hd:_) -> worlds !! ((read $ hd) :: Int)

    let aspectRatio :: Double = 16.0 / 9.0
    let imageWidth :: Integer = 400
    let samplesPerPixel :: Integer = 100
    let maxDepth :: Integer = 50
    let camera :: Camera = initCamera aspectRatio imageWidth samplesPerPixel maxDepth

    let ppm = render camera world
    putStrLn ppm
