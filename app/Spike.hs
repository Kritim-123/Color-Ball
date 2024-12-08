module Spike where

import Graphics.Gloss
import Circle

data Spike = Spike {
    spikePosition :: Point,  -- (x, y) position
    spikeColor    :: Color   -- Current color of the spike
} deriving (Eq, Show)

processCollisionsWithSpikes :: Circle -> [Spike] -> (Circle, [Spike])
processCollisionsWithSpikes circle spikes =
    foldr (\spike (c, accSpikes) ->
              if spikeCollide c spike
              then (c { getColor = white }, accSpikes ++ [spike { spikeColor = getColor c }])
              else (c, accSpikes ++ [spike]))
          (circle, []) spikes

spikeCollide :: Circle -> Spike -> Bool
spikeCollide (MkCircle x y r _ _ _) (Spike (sx, sy) _) =
    (sx - x)^2 + (sy - y)^2 <= r^2
