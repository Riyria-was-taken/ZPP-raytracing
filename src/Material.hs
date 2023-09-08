{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Material where

import           Hittable                      (HitRecord (..))
import           Ray                           (Ray (..))
import           System.Random.Mersenne.Pure64 (PureMT)
import           Utils                         (Color, Material (..), dot,
                                                nearZero, randomUnitVector,
                                                reflect, unit, (.*), (.+))

scatter :: PureMT -> Ray -> HitRecord -> Maybe (Ray, Color, PureMT)
scatter rng r rec =
    case rec.material of
        Lambertian { albedo } ->
            let (randomUnitVec3, rng1) = randomUnitVector rng in
            let scatterDirection = rec.normal .+ randomUnitVec3 in
            let scattered = Ray { origin = rec.p, direction = if nearZero scatterDirection then rec.normal else scatterDirection } in
            Just (scattered, albedo, rng1)
        Metal { albedo, fuzz } ->
            let (randomUnitVec3, rng1) = randomUnitVector rng in
            let reflected = reflect (unit r.direction) rec.normal in
            let scattered = Ray { origin = rec.p, direction = reflected .+ randomUnitVec3 .* fuzz } in
            case dot scattered.direction rec.normal > 0 of
                True  -> Just (scattered, albedo, rng1)
                False -> Nothing
