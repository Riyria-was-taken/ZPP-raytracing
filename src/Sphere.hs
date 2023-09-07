{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sphere where

import           Debug.Trace (trace)
import           Ray         (Ray (..))
import           Utils       (Point (..), Vec3 (..), dot, len, (.-))
data Sphere = Sphere { center :: Point, radius :: Double } deriving (Eq, Show)

hitSphere :: Sphere -> Ray -> Double
hitSphere s r =
    let oc = r.origin .- s.center in
    let a = lenSquared r.direction in
    let halfB = dot oc r.direction in
    let c = lenSquared oc - s.radius * s.radius in
    let discriminant = halfB * halfB - a * c in
    case discriminant < 0 of
        True  -> -1.0
        False -> (-halfB - sqrt discriminant) / a
