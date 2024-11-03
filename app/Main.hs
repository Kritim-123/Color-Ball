module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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
            -- Random initial velocity for each circle (e.g., between -2 and 2 for both x and y)
            vx = (-1) + fromIntegral (clickCount `mod` 4) -- Change this as needed for different velocities
            vy = 1 + fromIntegral (clickCount `mod` 4)
            newCircle = (mouseX, mouseY, radius, newCircleColor, vx, vy)
        in GameState (newCircle : circles) (clickCount + 1)
    eventHandler _ state = state

    -- Update the state by moving each circle
    update :: Float -> GameState -> GameState
    update _ (GameState circles clickCount) = 
        GameState (map moveCircle circles) clickCount

    -- Move each circle based on its velocity, and bounce off edges
    moveCircle :: Circle -> Circle
    moveCircle (x, y, r, c, vx, vy) =
        let newX = x + vx
            newY = y + vy
            -- Bounce off the walls by inverting the velocity if out of bounds
            newVx = if newX + r > 300 || newX - r < -300 then -vx else vx
            newVy = if newY + r > 300 || newY - r < -300 then -vy else vy
        in (newX, newY, r, c, newVx, newVy)

