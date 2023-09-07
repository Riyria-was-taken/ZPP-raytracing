{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           Control.Monad (forM_)
import           Ray           (Ray (..), at)
import           Sphere        (Sphere (..), hitSphere)
import           System.IO     (hPutStrLn, putStrLn, stderr)
import           Text.Printf   (printf)
import           Utils         (Color (..), Point (..), Vec3 (..), colorSpace,
                                printPixel, unit, zeroesVec3, (.*), (.+), (.-),
                                (./))

rayColor :: Ray -> Color
rayColor r =
    let t = hitSphere Sphere { center = Vec3 {x = 0, y = 0, z = -1}, radius = 0.5 } r in
    case t > 0.0 of
        True ->
            let n :: Vec3 = unit $ at r t .- Vec3 {x = 0, y = 0, z = -1 } in
            (n .+ Vec3 {x = 1, y = 1, z = 1}) .* 0.5
        False ->
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

    -- TODO żeby kolejność odejmowania działała
    let viewportUpperLeft :: Point = ((cameraCenter .- Vec3 { x = 0, y = 0, z = focalLength }) .- viewportU ./ 2) .- viewportV ./ 2
    let pixel00Loc :: Point = viewportUpperLeft .+ (pixelDeltaU .+ pixelDeltaV) .* 0.5

    forM_ [0..imageHeight-1] (\j -> do
        hPutStrLn stderr $ printf "Remaining lines: %d" (imageHeight - j + 1)
        forM_ [0..imageWidth-1] (\i -> do
            let pixelCenter :: Point = pixel00Loc .+ pixelDeltaU .* fromInteger i .+ pixelDeltaV .* fromInteger j
            let ray :: Ray = Ray { origin = cameraCenter, direction = pixelCenter .- cameraCenter }
            let color :: Color = rayColor ray
            printPixel color))
    hPutStrLn stderr "Done!"
