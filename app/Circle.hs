module Circle where

import Graphics.Gloss

coloredCircle :: Color -> Picture
coloredCircle col = color col (circleSolid 5)

