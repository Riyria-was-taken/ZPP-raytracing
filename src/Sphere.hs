{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sphere where

import           Debug.Trace (trace)
import           Ray         (Ray (..))
import           Utils       (Point (..), Vec3 (..), dot, len, (.-))
data Sphere = Sphere { center :: Point, radius :: Double } deriving (Eq, Show)

hitSphere :: Sphere -> Ray -> Bool
hitSphere s r =
    let oc = r.origin .- s.center in
    let a = dot r.direction r.direction in
    let b = 2.0 * dot oc r.direction in
    let c = dot oc oc - s.radius * s.radius in
    let discriminant = b * b - 4.0 * a * c in
    discriminant >= 0.0
