{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           HittableList  (HittableList (..))
import           Sphere        (Sphere (..))
import           System.IO     (putStrLn)
import           Utils         (Point, Vec3 (..))
import           Camera        (Camera, initCamera, render)
import System.Random

main :: IO ()
main = do
    --args <- getArgs
    --case args of
    --  [] -> getContents >>= run
    --  fs -> foldM mergeFile "" (reverse fs) >>= run
    
    let world :: HittableList = [Sphere { center = Vec3 {x = 0, y = 0, z = -1 }, radius = 0.5 },
                 Sphere { center = Vec3 {x = 0, y = -100.5, z = -1}, radius = 100}]

    let aspectRatio :: Double = 16.0 / 9.0
    let imageWidth :: Integer = 400
    let samplesPerPixel :: Integer = 10
    let maxDepth :: Integer = 50
    let camera :: Camera = initCamera aspectRatio imageWidth samplesPerPixel maxDepth

    let ppm = render camera world
    putStrLn ppm
