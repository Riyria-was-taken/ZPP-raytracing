{-# LANGUAGE OverloadedRecordDot #-}

module Hittable where

import           Interval (Interval (..))
import           Ray      (Ray (..))
import           Utils    (Point, Vec3, dot, neg)

data HitRecord = HitRecord { p :: Point, normal :: Vec3, t :: Double, frontFace :: Bool } deriving (Eq, Show)

setFaceNormal :: HitRecord -> Ray -> HitRecord
setFaceNormal rec r =
    let frontFace = dot r.direction rec.normal < 0 in
    rec { frontFace = frontFace, normal = if frontFace then rec.normal else neg rec.normal }

class Hittable a where
    hit :: a -> Ray -> Interval -> Maybe HitRecord
