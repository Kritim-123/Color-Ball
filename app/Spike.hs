module Spike where

data Spike = MkSpike
 {
  getXPos :: Float, 
  getYPos :: Float
}
  deriving (Eq, Show)
