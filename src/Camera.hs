{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camera where

import           Control.Monad (forM_)
import           Hittable      (HitRecord (..), Hittable (..))
import           HittableList  (HittableList (..))
import           Ray           (Ray (..))
import           Text.Printf   (printf)
import           Utils         (Color (..), Point (..), Vec3 (..), infinity,
                                unit, zeroesVec3, (.*), (.+), (.-), (./))

data Camera = Camera {aspectRatio :: Double,
                      imageWidth, imageHeight, colorSpace :: Integer,
                      center, pixel00Loc :: Point,
                      pixelDeltaU, pixelDeltaV :: Vec3 }

initCamera :: Double -> Integer -> Camera
initCamera aspectRatio imageWidth =
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
             imageWidth = imageWidth, imageHeight = imageHeight, colorSpace = 256,
             center = cameraCenter, pixel00Loc = pixel00Loc,
             pixelDeltaU = pixelDeltaU, pixelDeltaV = pixelDeltaV }

render :: Camera -> HittableList -> String
render cam world =
    let out = foldr (\j str -> foldr (\i str -> processPixel j i cam world ++ str) str [0..cam.imageWidth-1]) "" [0..cam.imageHeight-1] in
    printf "P3\n%d %d\n%d\n" cam.imageWidth cam.imageHeight cam.colorSpace ++ out

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

processPixel :: Integer -> Integer -> Camera -> HittableList -> String
processPixel j i cam world =
    let pixelCenter :: Point = cam.pixel00Loc .+ cam.pixelDeltaU .* fromInteger i .+ cam.pixelDeltaV .* fromInteger j in
    let ray :: Ray = Ray { origin = cam.center, direction = pixelCenter .- cam.center } in
    let color :: Color = rayColor ray world in
    let r :: Integer = round (color.x * fromInteger cam.colorSpace) in
    let g :: Integer = round (color.y * fromInteger cam.colorSpace) in
    let b :: Integer = round (color.z * fromInteger cam.colorSpace) in
    printf "%d %d %d\n" r g b

