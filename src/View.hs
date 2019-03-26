--- Copyright 2019 The Australian National University, All rights reserved

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
  LineTool _      -> "Line... click-drag-release"
  PolygonTool _   -> "Polygon... click 3 or more times then spacebar"
  RectangleTool _ -> "Rectangle... click-drag-release"
  CircleTool _    -> "Circle... click-drag-release"
  EllipseTool _   -> "Ellipse... click-drag-release"

-- | Returns a Picture of Shape coloured ColourName from ColourShape.
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture colourShapeList = case colourShapeList of
  [] -> mempty
  x:xs -> colourShapeToPicture x & colourShapesToPicture xs

-- | Returns a Picture of Shape coloured ColourName from ColourShape.
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture (colourName, shape) = coloured (colourNameToColour colourName) (shapeToPicture shape)

-- | Returns the (CodeWorld) Colour equivalent of ColourName.
colourNameToColour :: ColourName -> Colour
colourNameToColour colourName = case colourName of
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
  Line (x0,y0) (x1,y1)      -> polyline [(x0,y0),(x1,y1)]
  Polygon xList             -> solidPolygon xList
  Rectangle (x0,y0) (x1,y1) -> translated (midpoint x0 x1) (midpoint y0 y1) (solidRectangle (distance x0 x1) (distance y0 y1))
  Circle (x0,y0) (x1,y1)    -> translated x0 y0 (solidCircle (sqrt ((x1 - x0)**2 + (y1 - y0)**2)))
  Ellipse (x0,y0) (x1,y1)
    | (distance x0 x1) > (distance y0 y1) -> transScaleSolidCircle (midpoint x0 x1) (midpoint y0 y1) ((distance x0 x1)/(distance y0 y1)) 1 ((distance y0 y1)/2)
    | otherwise -> transScaleSolidCircle (midpoint x0 x1) (midpoint y0 y1) 1 ((distance y0 y1)/(distance x0 x1)) ((distance x0 x1)/2)
  where
    distance a b = abs (a - b)
    midpoint a b = (a + b) / 2
    transScaleSolidCircle transA transB scaleA scaleB raduis = translated transA transB (scaled scaleA scaleB (solidCircle raduis))

-- | Returns a picture that combines the input picture with a coordinatePlane, for testing and dimensioning purposes.
addCoordinatePlane :: Picture -> Picture
addCoordinatePlane picture = coordinatePlane & picture