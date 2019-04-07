--- Copyright 2019 The Australian National University, All rights reserved
{-

Name: Tim James
ID: u6947396

-}

module View where

import CodeWorld
import Data.Text (pack)
import Model

-- | Render all the parts of a Model to a CodeWorld picture.
modelToPicture :: Model -> Picture
modelToPicture (Model ss t c)
  = translated 0 8 toolText
  & translated 0 7 colourText
  & colourShapesToPicture ss
  & coordinatePlane
  where
    colourText = stringToText (show c)
    toolText = stringToText (toolLabel t)
    stringToText = text . pack

-- | Returns instructions on how to use each Tool.
toolLabel :: Tool -> String
toolLabel tool = case tool of
  -- Define the label to inform on how each tool is used.
  LineTool _      -> "Line... click-drag-release"
  PolygonTool _   -> "Polygon... click 3 or more times then spacebar"
  RectangleTool _ -> "Rectangle... click-drag-release"
  CircleTool _    -> "Circle... click-drag-release"
  EllipseTool _   -> "Ellipse... click-drag-release"

-- | Returns a Picture of Shape coloured ColourName from ColourShape.
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture colourShapeList = case colourShapeList of
  -- If no shaped are stored, the picture should be empty.
  [] -> mempty
  -- Otherwise, combine all elements of colourShapeList into a single picture.
  x:xs -> colourShapeToPicture x & colourShapesToPicture xs

-- | Returns a Picture of Shape coloured ColourName from ColourShape.
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture (colourName, shape) = coloured (colourNameToColour colourName) (shapeToPicture shape)

-- | Returns the (CodeWorld) Colour equivalent of ColourName.
colourNameToColour :: ColourName -> Colour
colourNameToColour colourName = case colourName of
  -- Each case of ColourName is matched against its Colour (CodeWorld) equivalent.
  Black     -> black
  Red       -> red
  Orange    -> orange
  Yellow    -> yellow
  Green     -> green
  Blue      -> blue
  Violet    -> violet

-- | Returns a Picture of Shape based on Point inputs.
shapeToPicture :: Shape -> Picture
shapeToPicture shape = case shape of
  -- Line is constructed with only two points in a polyline function, where each point is a vertex.
  Line (x0,y0) (x1,y1)      -> polyline [(x0,y0),(x1,y1)]

  -- Polygon is constructed through a list of points where each point is a vertex.
  Polygon xList             -> solidPolygon xList

  -- Rectangle is constructed through two points where each point is an opposite corner.
  Rectangle (x0,y0) (x1,y1) -> translated (midpoint x0 x1) (midpoint y0 y1) (solidRectangle (distance x0 x1) (distance y0 y1))

  -- Circle is constructed where the first point is the centre, and the second point is a point on the circumference.
  Circle (x0,y0) (x1,y1)    -> translated x0 y0 (solidCircle (sqrt ((x1 - x0)**2 + (y1 - y0)**2)))

  -- An ellipse is constructed with each point defining opposite corners of a bounding box.
  -- A circle is drawn within the bounding box, having a radius of half of the distance between the vertical axis of the points,
  -- and the horizontal axis is scaled by the ratio of the distance between horizontal axis points to the distance between vertical axis points.
  Ellipse (x0, y0) (x1, y1) -> translated (midpoint x0 x1) (midpoint y0 y1) (scaled ((distance x0 x1)/(distance y0 y1)) 1 (solidCircle ((distance y0 y1)/2)))

  where
    -- For use in finding the distance between point co-ordinates.
    distance :: Double -> Double -> Double
    distance a b = abs (a - b)

    -- For use in finding the midpoint between point co-ordinates.
    midpoint :: Double -> Double -> Double
    midpoint a b = (a + b) / 2

-- | Returns a picture that combines the input picture with a coordinatePlane, for testing and dimensioning purposes.
addCoordinatePlane :: Picture -> Picture
addCoordinatePlane picture = coordinatePlane & picture