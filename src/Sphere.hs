{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sphere where

import           Hittable (HitRecord (..), Hittable (..), setFaceNormal)
import           Ray      (Ray (..), at)
import           Utils    (Interval (..), Material, Point (..), Vec3 (..), dot,
                           len, lenSquared, surrounds, (.-), (./))

data Sphere = Sphere { center :: Point, radius :: Double, material :: Material } deriving (Eq, Show)

instance Hittable Sphere where
    hit s r interval =
        let oc = r.origin .- s.center in
        let a = lenSquared r.direction in
        let halfB = dot oc r.direction in
        let c = lenSquared oc - s.radius * s.radius in
        let discriminant = halfB * halfB - a * c in
        case discriminant < 0 of
            True -> Nothing
            False ->
                let sqrtDiscriminant = sqrt discriminant in
                let root1 = (-halfB - sqrtDiscriminant) / a in
                case surrounds interval root1 of
                    True -> Just $ buildHitRecord s r root1
                    False ->
                        let root2 = (-halfB + sqrtDiscriminant) / a in
                        case surrounds interval root2 of
                            True  -> Just $ buildHitRecord s r root2
                            False -> Nothing

buildHitRecord :: Sphere -> Ray -> Double -> HitRecord
buildHitRecord s r t =
    let p = at r t in
    let rec = HitRecord { p = p, normal = (p .- s.center) ./ s.radius, t = t, frontFace = True, material = s.material } in
    setFaceNormal rec r

