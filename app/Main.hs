module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (tails)
import Data.Maybe (catMaybes)

type Circle = (Float, Float, Float, Color, Float, Float) -- (x, y, radius, color, vx, vy)

data GameState = GameState {
    circles :: [Circle],
    clickCount :: Int
}

colors :: [Color]
colors = [red, blue, green, violet, aquamarine]

getColor :: Int -> Color
getColor clickCount = colors !! ((clickCount - 1) `mod` length colors)

main :: IO ()
main = play window bgColor fps initialState toPicture eventHandler update
  where
    window = InWindow "Floating Circles" (600, 600) (100, 100)
    bgColor = white
    fps = 60

    initialState :: GameState
    initialState = GameState [] 0

    -- Convert game state to a picture by drawing each circle
    toPicture :: GameState -> Picture
    toPicture (GameState circles _) =
        Pictures [translate x y (Color c (circleSolid r)) | (x, y, r, c, _, _) <- circles]

    -- Event handler for mouse clicks
    eventHandler :: Event -> GameState -> GameState
    eventHandler (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) (GameState circles clickCount) =
        let newCircleColor = getColor (clickCount + 1)
            radius = 30
            vx = (-1) + fromIntegral (clickCount `mod` 4)
            vy = 1 + fromIntegral (clickCount `mod` 4)
            newCircle = (mouseX, mouseY, radius, newCircleColor, vx, vy)
        in GameState (newCircle : circles) (clickCount + 1)
    eventHandler _ state = state

    -- Update the state by moving each circle and handling collisions
    update :: Float -> GameState -> GameState
    update _ (GameState circles clickCount) =
        let movedCircles = map moveCircle circles
            newCircles = handleCollisions movedCircles
        in GameState newCircles clickCount

    -- Move each circle based on its velocity, and bounce off edges
    moveCircle :: Circle -> Circle
    moveCircle (x, y, r, c, vx, vy) =
        let newX = x + vx
            newY = y + vy
            newVx = if newX + r > 300 || newX - r < -300 then -vx else vx
            newVy = if newY + r > 300 || newY - r < -300 then -vy else vy
        in (newX, newY, r, c, newVx, newVy)

    -- Detect and handle collisions between circles
    handleCollisions :: [Circle] -> [Circle]
    handleCollisions [] = []
    handleCollisions (c:cs) =
        let (mergedCircle, remainingCircles) = processCollisions c cs
        in mergedCircle : handleCollisions remainingCircles

    -- Process collisions for a given circle with the rest of the circles
    processCollisions :: Circle -> [Circle] -> (Circle, [Circle])
    processCollisions c [] = (c, [])
    processCollisions c (c2:cs) =
        if circlesCollide c c2
        then let combinedCircle = mergeCircles c c2
             in processCollisions combinedCircle cs -- Continue merging with remaining circles
        else let (nextCircle, remaining) = processCollisions c cs
             in (nextCircle, c2 : remaining)

    -- Check if two circles collide
    circlesCollide :: Circle -> Circle -> Bool
    circlesCollide (x1, y1, r1, _, _, _) (x2, y2, r2, _, _, _) =
        (x2 - x1)^2 + (y2 - y1)^2 <= (r1 + r2)^2

    -- Merge two circles into one with a new radius, position, and mixed color
    mergeCircles :: Circle -> Circle -> Circle
    mergeCircles (x1, y1, r1, c1, vx1, vy1) (x2, y2, r2, c2, vx2, vy2) =
        let newRadius = r1
            newX = x1
            newY = y1
            newColor = mixColors c1 c2
            newVx = (vx1 + vx2) / 2 -- Average velocity
            newVy = (vy1 + vy2) / 2
        in (newX, newY, newRadius, newColor, newVx, newVy)

    -- Blend two colors
    mixColors :: Color -> Color -> Color
    mixColors c1 c2 =
        let (r1, g1, b1, a1) = rgbaOfColor c1
            (r2, g2, b2, a2) = rgbaOfColor c2
            newR = (r1 + r2) / 2
            newG = (g1 + g2) / 2
            newB = (b1 + b2) / 2
            newA = (a1 + a2) / 2
        in makeColor newR newG newB newA

