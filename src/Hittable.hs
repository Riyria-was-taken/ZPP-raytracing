{-# LANGUAGE OverloadedRecordDot #-}

module Hittable where

import           Ray   (Ray (..))
import           Utils (Interval (..), Material, Point, Vec3, dot, neg)

data HitRecord = HitRecord { p :: Point, normal :: Vec3, t :: Double, material :: Material, frontFace :: Bool } deriving (Eq, Show)

setFaceNormal :: HitRecord -> Ray -> HitRecord
setFaceNormal rec r =
    let frontFace = dot r.direction rec.normal < 0 in
    rec { frontFace = frontFace, normal = if frontFace then rec.normal else neg rec.normal }

class Hittable a where
    hit :: a -> Ray -> Interval -> Maybe HitRecord
