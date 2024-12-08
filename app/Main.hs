module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Circle
import Spike

-- Define the game state
data GameState = GameState {
    circles :: [Circle],            -- List of circles
    clickCount :: Int,              -- Count of clicks
    animations :: [(Circle, [Color])], -- Active animations (circle and color transition)
    spikes :: [Spike]
}

-- Predefined colors for circles
colors :: [Color]
colors = [red, blue, green, violet, aquamarine]

-- Get a color based on the click count
getColorFromClick :: Int -> Color
getColorFromClick clickCount = colors !! ((clickCount - 1) `mod` length colors)

-- Extract color from a circle
extractColor :: Circle -> Color
extractColor (MkCircle _ _ _ color _ _) = color

-- Main function
main :: IO ()
main = play window bgColor fps initialState toPicture eventHandler update
  where
    window = InWindow "Floating Circles" (600, 600) (100, 100)
    bgColor = white
    fps = 60

    -- Initial game state
    initialState :: GameState
    initialState = GameState [] 0 [] initialSpikes
      where
        initialSpikes = [Spike (-200, 0) black, Spike (200, 0) black]  -- Example positions

    -- Render the game state to a picture
    -- toPicture :: GameState -> Picture
    -- toPicture (GameState circles _ animations) =
    --     Pictures $
    --         -- Render all static circles
    --         [circleToPicture circle | circle <- circles] ++
    --         -- Render circles currently animating with their interpolated color
    --         [circleToPicture (updateCircleColor c (head colors)) | (c, colors) <- animations, not (null colors)]

    toPicture :: GameState -> Picture
    toPicture (GameState circles _ animations spikes) =
        Pictures $ map circleToPicture circles ++
                map spikeToPicture spikes ++
                [circleToPicture (updateCircleColor c (head colors)) | (c, colors) <- animations, not (null colors)]

    spikeToPicture :: Spike -> Picture
    spikeToPicture (Spike (x, y) spikeColor) =
        translate x y $ color spikeColor $ pictures
            [rotate angle $ polygon [(0, 0), (10, 30), (-10, 30)] | angle <- [0, 90, 180, 270]]

    -- Handle mouse click events
    -- eventHandler :: Event -> GameState -> GameState
    -- eventHandler (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) (GameState circles clickCount animations) =
    --     let newCircleColor = getColorFromClick (clickCount + 1)
    --         radius = 30
    --         vx = (-1) + fromIntegral (clickCount `mod` 4)
    --         vy = 1 + fromIntegral (clickCount `mod` 4)
    --         newCircle = MkCircle mouseX mouseY radius newCircleColor vx vy
    --     in GameState (newCircle : circles) (clickCount + 1) animations
    -- eventHandler _ state = state

    eventHandler :: Event -> GameState -> GameState
    eventHandler (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) (GameState circles clickCount animations spikes) =
        let newCircleColor = getColorFromClick (clickCount + 1)
            radius = 30
            vx = (-1) + fromIntegral (clickCount `mod` 4)
            vy = 1 + fromIntegral (clickCount `mod` 4)
            newCircle = MkCircle mouseX mouseY radius newCircleColor vx vy
        in GameState (newCircle : circles) (clickCount + 1) animations spikes  -- Include spikes in the updated state
    eventHandler _ gameState =
        -- Handle all other events; this does nothing and returns the game state unchanged
        gameState

    -- Update game state (move circles, handle collisions, and update animations)
    update :: Float -> GameState -> GameState
    update _ (GameState circles clickCount animations spikes) =
      let movedCircles = map moveCircle circles
        -- Handle collisions with spikes (returns updated circles and spikes)
        (remainingCircles, updatedSpikes) = processCollisionsWithSpikes movedCircles spikes
        -- Handle collisions between circles (merging and animations)
        (finalCircles, updatedAnimations) = handleCollisions remainingCircles animations
        -- Update animations
        finalAnimations = updateAnimations updatedAnimations
      in GameState finalCircles clickCount finalAnimations updatedSpikes

  --  update :: Float -> GameState -> GameState
  --  update _ gameState@(GameState circles clickCount animations spikes) =
  --      let movedCircles = map moveCircle circles
  --          -- Make sure that processCircle is used correctly with foldr
  --          (newCircles, newAnimations, newSpikes) = foldr processCircle ([], animations, spikes) movedCircles
  --      in GameState newCircles clickCount newAnimations newSpikes

    processCircle :: Circle -> ([Circle], [(Circle, [Color])], [Spike]) -> ([Circle], [(Circle, [Color])], [Spike])
    processCircle circle (circles, animations, spikes) =
        let (updatedCircle, updatedSpikes) = processCollisionsWithSpikes circle spikes
        in (updatedCircle : circles, animations, updatedSpikes ++ spikes)

    -- Move a circle based on its velocity and bounce off edges
    moveCircle :: Circle -> Circle
    moveCircle (MkCircle x y r c vx vy) =
        let newX = x + vx
            newY = y + vy
            newVx = if newX + r > 300 || newX - r < -300 then -vx else vx
            newVy = if newY + r > 300 || newY - r < -300 then -vy else vy
        in MkCircle newX newY r c newVx newVy

    -- Handle collisions and generate animations for the bigger circle
    handleCollisions :: [Circle] -> [(Circle, [Color])] -> ([Circle], [(Circle, [Color])])
    handleCollisions [] animations = ([], animations)
    handleCollisions (c:cs) animations =
        let (processedCircle, remainingCircles, newAnimations) = processCollisions c cs animations
            (restCircles, restAnimations) = handleCollisions remainingCircles newAnimations
        in (processedCircle : restCircles, restAnimations)

    -- Process collisions for a given circle
    processCollisions :: Circle -> [Circle] -> [(Circle, [Color])] -> (Circle, [Circle], [(Circle, [Color])])
    processCollisions c [] animations = (c, [], animations)
    processCollisions c (c2:cs) animations
        | circlesCollide c c2 =
            let biggerCircle = if getRadius c >= getRadius c2 then c else c2
                smallerCircle = if getRadius c < getRadius c2 then c else c2
                combinedColor = mixColors (extractColor biggerCircle) (extractColor smallerCircle)
                colorFrames = interpolateColors (extractColor biggerCircle) combinedColor
                newAnimations = (biggerCircle, colorFrames) : animations
            in (c {getColor = combinedColor}, cs, newAnimations)
        | otherwise =
            let (nextCircle, remaining, anims) = processCollisions c cs animations
            in (nextCircle, c2 : remaining, anims)

    -- Check if two circles collide
    circlesCollide :: Circle -> Circle -> Bool
    circlesCollide (MkCircle x1 y1 r1 _ _ _) (MkCircle x2 y2 r2 _ _ _) =
        (x2 - x1)^2 + (y2 - y1)^2 <= (r1 + r2)^2

    -- Update animations by advancing color frames
    updateAnimations :: [(Circle, [Color])] -> [(Circle, [Color])]
    updateAnimations = map (\(c, colors) -> (c, tail colors)) . filter (not . null . snd)

    -- Interpolate between two colors to create a list of colors
    interpolateColors :: Color -> Color -> [Color]
    interpolateColors c1 c2 = [blendColors c1 c2 t | t <- [0, 0.05 .. 1]]

    -- Blend two colors into one
    mixColors :: Color -> Color -> Color
    mixColors c1 c2 =
        let (r1, g1, b1, a1) = rgbaOfColor c1
            (r2, g2, b2, a2) = rgbaOfColor c2
        in makeColor ((r1 + r2) / 2) ((g1 + g2) / 2) ((b1 + b2) / 2) ((a1 + a2) / 2)

    -- Blend two colors based on interpolation factor t
    blendColors :: Color -> Color -> Float -> Color
    blendColors c1 c2 t =
        let (r1, g1, b1, a1) = rgbaOfColor c1
            (r2, g2, b2, a2) = rgbaOfColor c2
            blend x1 x2 = x1 * (1 - t) + x2 * t
        in makeColor (blend r1 r2) (blend g1 g2) (blend b1 b2) (blend a1 a2)

    -- Update the color of a circle
    updateCircleColor :: Circle -> Color -> Circle
    updateCircleColor (MkCircle x y r _ vx vy) newColor = MkCircle x y r newColor vx vy

    -- Convert a circle to a picture
    circleToPicture :: Circle -> Picture
    circleToPicture (MkCircle x y r color _ _) =
        translate x y (Color color (circleSolid r))

