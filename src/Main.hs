{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           Control.Monad (forM_)
import           Ray           (Ray (..))
import           System.IO     (hPutStrLn, putStrLn, stderr)
import           Text.Printf   (printf)
import           Utils         (Color (..), Point (..), Vec3 (..), colorSpace,
                                printPixel, unit, zeroesVec3, (.*), (.+), (.-),
                                (./))

rayColor :: Ray -> Color
rayColor r =
    let unitDirection :: Vec3 = unit r.direction in
    let a :: Double = 0.5 * (unitDirection.y + 1.0) in
    let white :: Color = Vec3 {x = 1.0, y = 1.0, z = 1.0} in
    let blue :: Color = Vec3 {x = 0.5, y = 0.7, z =  1.0} in
    white .* (1.0 - a) .+ blue .* a

main :: IO ()
main = do
    --args <- getArgs
    --case args of
    --  [] -> getContents >>= run
    --  fs -> foldM mergeFile "" (reverse fs) >>= run
    putStrLn "P3"
    let aspectRatio :: Double = 16.0 / 9.0
    let imageWidth :: Integer = 400
    let imageHeight :: Integer = max 1 $ round $ fromInteger imageWidth / aspectRatio
    putStrLn $ printf "%d %d" imageWidth imageHeight
    putStrLn $ show colorSpace

    let focalLength :: Double = 1.0
    let viewportHeight :: Double = 2.0
    let viewportWidth :: Double = viewportHeight * (fromInteger imageWidth / fromInteger imageHeight)
    let cameraCenter :: Point = zeroesVec3

    let viewportU :: Vec3 = Vec3 { x = viewportWidth, y = 0, z = 0 }
    let viewportV :: Vec3 = Vec3 { x = 0, y = -viewportHeight, z = 0 }

    let pixelDeltaU :: Vec3 = viewportU ./ fromInteger imageWidth
    let pixelDeltaV :: Vec3 = viewportV ./ fromInteger imageHeight

    let viewportUpperLeft :: Point = cameraCenter .- Vec3 { x = 0, y = 0, z = focalLength } .- viewportU ./ 2 .- viewportV ./ 2
    let pixel00Loc :: Point = viewportUpperLeft .+ (pixelDeltaU .+ pixelDeltaV) .* 0.5

    forM_ [1..imageHeight] (\i -> do
        hPutStrLn stderr $ printf "Remaining lines: %d" (imageHeight - i + 1)
        forM_ [1..imageWidth] (\j -> do
            let pixelCenter :: Point = pixel00Loc .+ pixelDeltaU .* fromInteger i .+ pixelDeltaV .* fromInteger j
            let ray :: Ray = Ray { origin = cameraCenter, direction = pixelCenter .- cameraCenter }
            let color :: Color = rayColor ray
            printPixel color))
    hPutStrLn stderr "Done!"
