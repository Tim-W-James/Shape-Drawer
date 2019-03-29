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
  -- Define the label depending on tool.
  LineTool _      -> "Line... click-drag-release"
  PolygonTool _   -> "Polygon... click 3 or more times then spacebar"
  RectangleTool _ -> "Rectangle... click-drag-release"
  CircleTool _    -> "Circle... click-drag-release"
  EllipseTool _   -> "Ellipse... click-drag-release"

-- | Returns a Picture of Shape coloured ColourName from ColourShape.
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture colourShapeList = case colourShapeList of
  -- If colourShapeList is empty, return an empty Picture (base case).
  [] -> mempty
  -- Otherwise, combine the first element of colourShapeList recursively with every other element in colourShapeList.
  -- Each element is converted from a ColourShape into a picture.
  x:xs -> colourShapeToPicture x & colourShapesToPicture xs

-- | Returns a Picture of Shape coloured ColourName from ColourShape.
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture (colourName, shape) = coloured (colourNameToColour colourName) (shapeToPicture shape)

-- | Returns the (CodeWorld) Colour equivalent of ColourName.
colourNameToColour :: ColourName -> Colour
colourNameToColour colourName = case colourName of
  -- Each case of ColourName is matched against its Colour equivalent.
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
  -- Line utilizes the polyline function, using only two points.
  Line (x0,y0) (x1,y1)      -> polyline [(x0,y0),(x1,y1)]

  -- Polygon uses a list of points and the function solidPolygon.
  Polygon xList             -> solidPolygon xList

  -- Rectangle applies the width and height based on the distance between each point to solidRectangle, and is translated to the midpoint between both points.
  Rectangle (x0,y0) (x1,y1) -> translated (midpoint x0 x1) (midpoint y0 y1) (solidRectangle (distance x0 x1) (distance y0 y1))

  -- Circle is translated to the first point, and solidCircle is applied to the radius calculated with the second point being on the circumference.
  Circle (x0,y0) (x1,y1)    -> translated x0 y0 (solidCircle (sqrt ((x1 - x0)**2 + (y1 - y0)**2)))

  -- An ellipse is constructed by applying solidCircle to the radius calculated with half of the distance between the major axis of the points,
  -- then the major axis is scaled by the ratio of the distance between major axis points to the distance between minor axis points,
  -- and is finally translated to the midpoint between both points.
  -- Note the use of helper functions.
  Ellipse (x0,y0) (x1,y1)
  -- Horizontal axis is major axis.
    | (distance x0 x1) > (distance y0 y1) -> transScaleSolidCircle (midpoint x0 x1) (midpoint y0 y1) ((distance x0 x1)/(distance y0 y1)) 1 ((distance y0 y1)/2)
  -- Vertical axis is major axis, or both axes are the same length.
    | otherwise -> transScaleSolidCircle (midpoint x0 x1) (midpoint y0 y1) 1 ((distance y0 y1)/(distance x0 x1)) ((distance x0 x1)/2)

  where
    -- Returns the absolute difference of two variables.
    distance :: Double -> Double -> Double
    distance a b = abs (a - b)
    -- Returns half of the sum of two variables.
    midpoint :: Double -> Double -> Double
    midpoint a b = (a + b) / 2
    -- Returns a Picture of a solidCircle that is translated and scaled. For use with drawing an Ellipse.
    transScaleSolidCircle :: Double -> Double -> Double -> Double -> Double -> Picture
    transScaleSolidCircle transA transB scaleA scaleB raduis = translated transA transB (scaled scaleA scaleB (solidCircle raduis))

-- | Returns a picture that combines the input picture with a coordinatePlane, for testing and dimensioning purposes.
addCoordinatePlane :: Picture -> Picture
addCoordinatePlane picture = coordinatePlane & picture