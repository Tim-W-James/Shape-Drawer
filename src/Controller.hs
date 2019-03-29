--- Copyright 2019 The Australian National University, All rights reserved
{-

Name: Tim James
ID: u6947396

-}

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event m@(Model ss t c) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> initialModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show m)) m

      -- display the mystery image
      | k == "M" -> Model mystery t c

      -- drop the last added shape
      | k == "Backspace" || k == "Delete" -> case ss of
        -- If list of ColourShapes ss in Model is empty, do nothing.
        [] -> Model ss t c
        -- Otherwise, remove the last added ColourShape.
        _ -> Model (tail ss) t c

      -- finish polygon vertices
      | k == " " -> case t of
        -- Builds a polygon only if PolygonTool is selected.
        PolygonTool pointList
          -- A polygon must have at least 3 points, so if pointList has less than 3 members, do nothing.
          | (length pointList) < 3 -> Model ss t c
          -- If pointList has at least 3 members, add the ColourShape to the list of ColourShapes ss in Model.
          | otherwise -> Model ((c,(Polygon pointList)):ss) (PolygonTool []) c
        -- Otherwise, do nothing.
        _ -> Model ss t c

      -- switch tool
      | k == "T" -> Model ss (nextTool t) c

      -- switch colour
      | k == "C" -> Model ss t (nextColour c)

      -- ignore other events
      | otherwise -> m
      where k = unpack key

    PointerPress p -> case t of
      -- If PolygonTool, add Point p to the list of Points stored in the Tool.
      PolygonTool pointList             -> Model ss (PolygonTool (p : pointList)) c
      -- Otherwise, add Point p to the held Point of the selected Tool.
      LineTool _                        -> Model ss (LineTool (Just p)) c
      RectangleTool _                   -> Model ss (RectangleTool (Just p)) c
      CircleTool _                      -> Model ss (CircleTool (Just p)) c
      EllipseTool _                     -> Model ss (EllipseTool (Just p)) c

    PointerRelease p -> case t of
      -- If PolygonTool, do nothing.
      PolygonTool _                     -> Model ss t c
      -- Otherwise, add ColourShape to the list of all ColourShapes ss in Model from selected Tool, its held Point, and the new Point p.
      -- Also reset held Point in Tool for future Shapes.
      LineTool (Just firstPoint)        -> Model ((c,(Line firstPoint p)):ss) (LineTool Nothing) c
      RectangleTool (Just firstPoint)   -> Model ((c,(Rectangle firstPoint p)):ss) (RectangleTool Nothing) c
      CircleTool (Just firstPoint)      -> Model ((c,(Circle firstPoint p)):ss) (CircleTool Nothing) c
      EllipseTool (Just firstPoint)     -> Model ((c,(Ellipse firstPoint p)):ss) (EllipseTool Nothing) c
      _                                 -> Model ss t c
    _ -> m

-- | Returns the next colour from the set of ColourNames.
nextColour :: ColourName -> ColourName
nextColour colourName = case colourName of
  -- At Violet, return Black to cycle back.
  Black     -> Red
  Red       -> Orange
  Orange    -> Yellow
  Yellow    -> Green
  Green     -> Blue
  Blue      -> Violet
  Violet    -> Black

-- | Returns the next Tool if no point is held, otherwise returns the argument unchanged.
nextTool :: Tool -> Tool
nextTool tool = case tool of
  -- If tool is not holding any point/s, return the next tool.
  -- At EllipseTool, return LineTool to cycle back.
  LineTool Nothing      -> PolygonTool []
  PolygonTool []        -> RectangleTool Nothing
  RectangleTool Nothing -> CircleTool Nothing
  CircleTool Nothing    -> EllipseTool Nothing
  EllipseTool Nothing   -> LineTool Nothing
  -- If tool is holding point/s, return the tool unchanged.
  LineTool x            -> LineTool x
  PolygonTool x         -> PolygonTool x
  RectangleTool x       -> RectangleTool x
  CircleTool x          -> CircleTool x
  EllipseTool x         -> EllipseTool x