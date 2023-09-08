{-# LANGUAGE OverloadedRecordDot #-}

module HittableList where

import           Hittable (HitRecord (..), Hittable (..))
import           Utils (Interval (..))
import           Ray      (Ray)
import           Sphere   (Sphere)

type HittableList = [Sphere]

instance Hittable HittableList where
    hit l r (tMin, tMax) =
        let (_, out) = foldr (\obj (closest, rec)->
                        case hit obj r (tMin, closest) of
                            Nothing     -> (closest, rec)
                            Just newRec -> (newRec.t, Just newRec)) (tMax, Nothing) l in
        out
