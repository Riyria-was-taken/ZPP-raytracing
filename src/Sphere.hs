{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sphere where

import           Hittable (HitRecord (..), Hittable (..), setFaceNormal)
import           Ray      (Ray (..), at)
import           Utils    (Point (..), Vec3 (..), dot, len, lenSquared, (.-),
                           (./))

data Sphere = Sphere { center :: Point, radius :: Double } deriving (Eq, Show)

instance Hittable Sphere where
    hit s r tMin tMax =
        let oc = r.origin .- s.center in
        let a = lenSquared r.direction in
        let halfB = dot oc r.direction in
        let c = lenSquared oc - s.radius * s.radius in
        let discriminant = halfB * halfB - a * c in
        case discriminant < 0 of
            True  -> Nothing
            False ->
                let sqrtDiscriminant = sqrt discriminant in
                let root1 = (-halfB - sqrtDiscriminant) / a in
                let root2 = (-halfB + sqrtDiscriminant) / a in
                case root1 <= tMin || tMax <= root1 of
                    True -> case root2 <= tMin || tMax <= root2 of
                                True  -> Nothing
                                False -> Just $ buildHitRecord s r root2
                    False -> Just $ buildHitRecord s r root1

buildHitRecord :: Sphere -> Ray -> Double -> HitRecord
buildHitRecord s r t =
    let p = at r t in
    let rec = HitRecord { p = p, normal = (p .- s.center) ./ s.radius, t = t, frontFace = True } in
    setFaceNormal rec r

