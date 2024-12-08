module Spike where

import Graphics.Gloss
import Circle

data Spike = Spike {
    spikePosition :: Point,  -- (x, y) position
    spikeColor    :: Color,   -- Current color of the spike
    spikeRadius   :: Float
} deriving (Eq, Show)

processCollisionsWithSpikes :: [Circle] -> [Spike] -> ([Circle], [Spike])
processCollisionsWithSpikes circles spikes =
    foldr processCollision ([], spikes) circles
  where
    processCollision :: Circle -> ([Circle], [Spike]) -> ([Circle], [Spike])
    processCollision circle (remainingCircles, updatedSpikes) =
        let (collided, newSpikes) = foldr (checkSpikeCollision circle) (False, []) updatedSpikes
        in if collided
           then (remainingCircles, newSpikes) -- Circle is removed
           else (circle : remainingCircles, newSpikes) -- Circle is kept

    checkSpikeCollision :: Circle -> Spike -> (Bool, [Spike]) -> (Bool, [Spike])
    checkSpikeCollision (MkCircle x y r color _ _) (Spike (sx, sy) spikeColor rad) (collided, accSpikes)
        | (sx - x)^2 + (sy - y)^2 <= (r + rad)^2 =
            (True, Spike (sx, sy) color rad: accSpikes) -- Update spike with circle's color
        | otherwise =
            (collided, Spike (sx, sy) spikeColor rad: accSpikes) -- Keep spike unchanged

