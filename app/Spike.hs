module Spike where

import Graphics.Gloss
import Circle

data Spike = MkSpike
 {
  getXPos :: Float, 
  getYPos :: Float,
  getColor :: Color
}
  deriving (Eq, Show)

-- Check if a spike contacts a circle
spikeContactsCircle :: Spike -> Circle -> Bool
spikeContactsCircle (MkSpike x1 y1 _) (MkCircle x2 y2 r2 _ _ _) =
    (x2 - x1)^2 + (y2 - y1)^2 <= r2^2