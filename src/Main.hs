{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Camera          (Camera, initCamera, render)
import           Control.DeepSeq
import           HittableList    (HittableList (..))
import           Sphere          (Sphere (..))
import           System.IO       (putStrLn)
import           System.Random
import           Utils           (Material (..), Point, Vec3 (..))

main :: IO ()
main = do
    --args <- getArgs
    --case args of
    --  [] -> getContents >>= run
    --  fs -> foldM mergeFile "" (reverse fs) >>= run

    let materialGround = Lambertian { albedo = Vec3 { x = 0.7, y = 0.8, z = 0.0 } }
    let materialCenter = Lambertian { albedo = Vec3 { x = 0.8, y = 0.3, z = 0.3 } }
    let materialLeft = Metal { albedo = Vec3 { x = 0.8, y = 0.8, z = 0.8 }, fuzz = 0.3 }
    let materialRight = Metal { albedo = Vec3 { x = 0.8, y = 0.6, z = 0.2 }, fuzz = 1.0 }

    let world :: HittableList = [Sphere { center = Vec3 {x = 0, y = 0, z = -1 }, radius = 0.5, material = materialCenter },
                 Sphere { center = Vec3 {x = 0, y = -100.5, z = -1}, radius = 100, material = materialGround},
                 Sphere { center = Vec3 {x = -1, y = 0, z = -1}, radius = 0.5, material = materialLeft},
                 Sphere { center = Vec3 {x = 1, y = 0, z = -1}, radius = 0.5, material = materialRight}]

    let aspectRatio :: Double = 16.0 / 9.0
    let imageWidth :: Integer = 800
    let samplesPerPixel :: Integer = 100
    let maxDepth :: Integer = 50
    let camera :: Camera = initCamera aspectRatio imageWidth samplesPerPixel maxDepth

    let ppm = render camera world
    putStrLn ppm
