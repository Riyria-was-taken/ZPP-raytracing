{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Ray where

import           Utils (Point (..), Vec3 (..), (.*), (.+))

data Ray = Ray { origin :: Point, direction :: Vec3 } deriving (Eq, Show)

at :: Ray -> Double -> Point
at r t = r.origin .+ r.direction .* t

