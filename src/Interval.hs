module Interval where

import           Utils (infinity)

type Interval = (Double, Double)

emptyInterval, universeInterval :: Interval
emptyInterval = (infinity, -infinity)
universeInterval = (-infinity, infinity)

contains :: Interval -> Double -> Bool
contains (min, max) x = min <= x && x <= max

surrounds :: Interval -> Double -> Bool
surrounds (min, max) x = min < x && x < max
