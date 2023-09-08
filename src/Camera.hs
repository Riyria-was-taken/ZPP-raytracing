{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camera where

import           Control.Concurrent
import           Control.Parallel.Strategies   (Eval, parMap, rpar, runEval)
import           Debug.Trace                   (trace)
import           Hittable                      (HitRecord (..), Hittable (..))
import           HittableList                  (HittableList (..))
import           Material                      (scatter)
import           Ray                           (Ray (..))
import           System.IO                     (putStrLn)
import           System.IO.Unsafe              (unsafePerformIO)
import           System.Random.Mersenne.Pure64 (PureMT, pureMT)
import           Text.Printf                   (printf)
import           Utils

data Camera = Camera {aspectRatio :: Double,
                      imageWidth, imageHeight, samplesPerPixel, colorSpace, maxDepth :: Integer,
                      center, pixel00Loc :: Point,
                      pixelDeltaU, pixelDeltaV :: Vec3 }

initCamera :: Double -> Integer -> Integer -> Integer -> Camera
initCamera aspectRatio imageWidth samplesPerPixel maxDepth =
    let imageHeight :: Integer = max 1 $ round $ fromInteger imageWidth / aspectRatio in
    let cameraCenter :: Point = zeroesVec3 in

    let focalLength :: Double = 1.0 in
    let viewportHeight :: Double = 2.0 in
    let viewportWidth :: Double = viewportHeight * (fromInteger imageWidth / fromInteger imageHeight) in

    let viewportU :: Vec3 = Vec3 { x = viewportWidth, y = 0, z = 0 } in
    let viewportV :: Vec3 = Vec3 { x = 0, y = -viewportHeight, z = 0 } in

    let pixelDeltaU :: Vec3 = viewportU ./ fromInteger imageWidth in
    let pixelDeltaV :: Vec3 = viewportV ./ fromInteger imageHeight in

    let viewportUpperLeft :: Point = ((cameraCenter .- Vec3 { x = 0, y = 0, z = focalLength }) .- viewportU ./ 2) .- viewportV ./ 2 in
    let pixel00Loc :: Point = viewportUpperLeft .+ (pixelDeltaU .+ pixelDeltaV) .* 0.5 in

    Camera { aspectRatio = aspectRatio,
             imageWidth = imageWidth, imageHeight = imageHeight, samplesPerPixel = samplesPerPixel, colorSpace = 256, maxDepth = maxDepth,
             center = cameraCenter, pixel00Loc = pixel00Loc,
             pixelDeltaU = pixelDeltaU, pixelDeltaV = pixelDeltaV }

render :: Camera -> HittableList -> String
render cam world =
    let parts = splitEvery 20 [0..cam.imageHeight-1] in
    let rows = concat $ parMap rpar (\part -> map (processRow cam world) part) parts in
    --let rows = runEval $ parMap (processRow cam world) [0..cam.imageHeight-1] in
    --putStrLn $ printf "P3\n%d %d\n%d\n" cam.imageWidth cam.imageHeight cam.colorSpace
    let out = concat rows in
    printf "P3\n%d %d\n%d\n" cam.imageWidth cam.imageHeight cam.colorSpace ++ out

processRow :: Camera -> HittableList -> Integer -> String
processRow cam world j =
    let newRng = pureMT $ fromInteger j in
    let (out, _) = foldr (\i (str, rng) -> let (pixelStr, rng1) = processPixel rng i j cam world in (pixelStr ++ str, rng1))
                         ("", newRng) [0..cam.imageWidth-1] in
    out

processPixel :: PureMT -> Integer -> Integer -> Camera -> HittableList -> (String, PureMT)
processPixel rng i j cam world =
    let (color, rng4) = foldr (\n (curr, rng1) ->
                                    let (r, rng2) = getRay rng1 cam i j in
                                    let (col, rng3) = rayColor rng2 r world cam.maxDepth in
                                    (curr .+ col, rng3)) (zeroesVec3, rng) [1..cam.samplesPerPixel] in
    (getColorStr color cam.colorSpace cam.samplesPerPixel, rng4)

getRay :: PureMT -> Camera -> Integer -> Integer -> (Ray, PureMT)
getRay rng cam i j =
    let pixelCenter :: Point = cam.pixel00Loc .+ cam.pixelDeltaU .* fromInteger i .+ cam.pixelDeltaV .* fromInteger j in
    let (pixelSampleDiff, rng1) = pixelSampleSquare rng cam in
    let pixelSample :: Point = pixelCenter .+ pixelSampleDiff in
    (Ray { origin = cam.center, direction = pixelSample .- cam.center }, rng1)

rayColor :: PureMT -> Ray -> HittableList -> Integer -> (Color, PureMT)
rayColor rng r world remainingDepth =
    case remainingDepth <= 0 of
        True -> (zeroesVec3, rng)
        False -> case hit world r (0.001, infinity) of
                    Just rec ->
                        case scatter rng r rec of
                            Just (scattered, attenuation, rng1) ->
                                let (color, rng2) = rayColor rng1 scattered world (remainingDepth - 1) in
                                (attenuation .** color, rng2)
                            Nothing -> (zeroesVec3, rng)
                    Nothing ->
                        let unitDirection :: Vec3 = unit r.direction in
                        let a :: Double = 0.5 * (unitDirection.y + 1.0) in
                        let white :: Color = Vec3 {x = 1.0, y = 1.0, z = 1.0} in
                        let blue :: Color = Vec3 {x = 0.5, y = 0.7, z =  1.0} in
                        (white .* (1.0 - a) .+ blue .* a, rng)

getColorStr :: Color -> Integer -> Integer -> String
getColorStr color colorSpace samples =
    let scale :: Double = 1.0 / fromInteger samples in
    let intensity :: Interval = (0.000, 0.999) in
    let r :: Integer = round (clamp intensity (linearToGamma $ color.x * scale) * fromInteger colorSpace) in
    let g :: Integer = round (clamp intensity (linearToGamma $ color.y * scale) * fromInteger colorSpace) in
    let b :: Integer = round (clamp intensity (linearToGamma $ color.z * scale) * fromInteger colorSpace) in
    printf "%d %d %d\n" r g b

pixelSampleSquare :: PureMT -> Camera -> (Vec3, PureMT)
pixelSampleSquare rng cam =
    let (x, rng1) = randomDouble rng in
    let (y, rng2) = randomDouble rng1 in
    let px = -0.5 * x in
    let py = -0.5 * y in
    (cam.pixelDeltaU .* px .+ cam.pixelDeltaV .* py, rng2)

--parMap :: (a -> b) -> [a] -> Eval [b]
--parMap f [] = return []
--parMap f (a:as) = do
--   b <- rpar (f a)
--   bs <- parMap f as
--   return (b:bs)
