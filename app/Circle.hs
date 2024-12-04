module Circle where

import Graphics.Gloss

data Circle = MkCircle
 {
  getX      ::  Float,
  getY      ::  Float,
  getRadius ::  Float,
  getColor  ::  Color,
  getXSpeed ::  Float,
  getYSpeed ::  Float
}
  deriving (Eq, Show)
