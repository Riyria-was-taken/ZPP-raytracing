{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camera where

import           Control.Monad (forM_, mapM_)
import           System.IO     (putStrLn)
import           Hittable      (HitRecord (..), Hittable (..))
import           HittableList  (HittableList (..))
import           Ray           (Ray (..))
import           Text.Printf   (printf)
import System.Random (StdGen, newStdGen, getStdGen)
import           Utils         (Color (..), Point (..), Vec3 (..), infinity, randomDouble,
                                unit, zeroesVec3, (.*), (.+), (.-), (./), Interval (..), clamp, splitEvery)
import Control.Parallel.Strategies (rpar, runEval, Eval)
import System.Random.MWC

data Camera = Camera {aspectRatio :: Double,
                      imageWidth, imageHeight, samplesPerPixel, colorSpace :: Integer,
                      center, pixel00Loc :: Point,
                      pixelDeltaU, pixelDeltaV :: Vec3 }

initCamera :: Double -> Integer -> Integer -> Camera
initCamera aspectRatio imageWidth samplesPerPixel =
    let imageHeight :: Integer = max 1 $ round $ fromInteger imageWidth / aspectRatio in
    let cameraCenter :: Point = zeroesVec3 in

    let focalLength :: Double = 1.0 in
    let viewportHeight :: Double = 2.0 in
    let viewportWidth :: Double = viewportHeight * (fromInteger imageWidth / fromInteger imageHeight) in

    let viewportU :: Vec3 = Vec3 { x = viewportWidth, y = 0, z = 0 } in
    let viewportV :: Vec3 = Vec3 { x = 0, y = -viewportHeight, z = 0 } in

    let pixelDeltaU :: Vec3 = viewportU ./ fromInteger imageWidth in
    let pixelDeltaV :: Vec3 = viewportV ./ fromInteger imageHeight in

    -- TODO żeby kolejność odejmowania działała
    let viewportUpperLeft :: Point = ((cameraCenter .- Vec3 { x = 0, y = 0, z = focalLength }) .- viewportU ./ 2) .- viewportV ./ 2 in
    let pixel00Loc :: Point = viewportUpperLeft .+ (pixelDeltaU .+ pixelDeltaV) .* 0.5 in

    Camera { aspectRatio = aspectRatio,
             imageWidth = imageWidth, imageHeight = imageHeight, samplesPerPixel = samplesPerPixel, colorSpace = 256,
             center = cameraCenter, pixel00Loc = pixel00Loc,
             pixelDeltaU = pixelDeltaU, pixelDeltaV = pixelDeltaV }

render :: Camera -> HittableList -> String
render cam world =
    let parts = splitEvery 50 [0..cam.imageHeight-1] in
    let rows = concat $ runEval $ parMap (\part -> map (processRow cam world) part) parts in
    --rows <- sequence $ runEval $ parMap (processRow cam world) [0..cam.imageHeight-1]
    --putStrLn $ printf "P3\n%d %d\n%d\n" cam.imageWidth cam.imageHeight cam.colorSpace
    let out = foldr (\row str -> row ++ str) "" rows in
    printf "P3\n%d %d\n%d\n" cam.imageWidth cam.imageHeight cam.colorSpace ++ out

processRow :: Camera -> HittableList -> Integer -> String
processRow cam world j = do
    --newGen <- getStdGen --initialize (singleton j)
    foldr (\i str ->  processPixel i j cam world ++ str) "" [0..cam.imageWidth-1]

processPixel :: Integer -> Integer -> Camera -> HittableList -> String
processPixel i j cam world =
    let color = foldr (\n c -> let r = getRay cam i j in --gen1 in
                                        c .+ rayColor r world) zeroesVec3 [1..cam.samplesPerPixel] in
    getColorStr color cam.colorSpace cam.samplesPerPixel

getRay :: Camera -> Integer -> Integer -> Ray -- StdGen -> (StdGen, Ray)
getRay cam i j = --gen =
    let pixelCenter :: Point = cam.pixel00Loc .+ cam.pixelDeltaU .* fromInteger i .+ cam.pixelDeltaV .* fromInteger j in
    --let (gen1, pixelSampleDiff) = pixelSampleSquare gen cam in
    --let pixelSample :: Point = pixelCenter .+ pixelSampleDiff in
    --(gen1, Ray { origin = cam.center, direction = pixelSample .- cam.center })
    Ray { origin = cam.center, direction = pixelCenter.- cam.center }

rayColor :: Ray -> HittableList -> Color
rayColor r world =
    case hit world r (0, infinity) of
        Just rec ->
            (rec.normal .+ Vec3 {x = 1, y = 1, z = 1}) .* 0.5
        Nothing ->
            let unitDirection :: Vec3 = unit r.direction in
            let a :: Double = 0.5 * (unitDirection.y + 1.0) in
            let white :: Color = Vec3 {x = 1.0, y = 1.0, z = 1.0} in
            let blue :: Color = Vec3 {x = 0.5, y = 0.7, z =  1.0} in
            white .* (1.0 - a) .+ blue .* a

getColorStr :: Color -> Integer -> Integer -> String
getColorStr color colorSpace samples =
    let scale :: Double = 1.0 / fromInteger samples in
    let intensity :: Interval = (0.000, 0.999) in
    let r :: Integer = round (clamp intensity (color.x * scale) * fromInteger colorSpace) in
    let g :: Integer = round (clamp intensity (color.y * scale) * fromInteger colorSpace) in
    let b :: Integer = round (clamp intensity (color.z * scale) * fromInteger colorSpace) in
    printf "%d %d %d\n" r g b

pixelSampleSquare :: StdGen -> Camera -> (StdGen, Vec3)
pixelSampleSquare gen cam =
    let (gen1, rnd1) = randomDouble gen in
    let (gen2, rnd2) = randomDouble gen1 in
    let px = -0.5 * rnd1 in
    let py = -0.5 * rnd2 in
    (gen2, cam.pixelDeltaU .* px .+ cam.pixelDeltaV .* py)

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)
