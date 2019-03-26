--- Copyright 2019 The Australian National University, All rights reserved

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

      | k == "Backspace" || k == "Delete" -> case ss of -- drop the last added shape
        [] -> Model ss t c
        _ -> Model (tail ss) t c
      | k == " " -> case t of -- finish polygon vertices
        PolygonTool pointList
          | (length pointList) < 3 -> Model ss t c -- pointList must have at least 3 members
          | otherwise -> Model ((c,(Polygon pointList)):ss) (PolygonTool []) c
        _ -> Model ss t c
      | k == "T" -> Model ss (nextTool t) c -- switch tool
      | k == "C" -> Model ss t (nextColour c) -- switch colour

      -- ignore other events
      | otherwise -> m
      where k = unpack key
    PointerPress p -> case t of
      PolygonTool pointList             -> Model ss (PolygonTool (p : pointList)) c
      LineTool _                        -> Model ss (LineTool (Just p)) c
      RectangleTool _                   -> Model ss (RectangleTool (Just p)) c
      CircleTool _                      -> Model ss (CircleTool (Just p)) c
      EllipseTool _                     -> Model ss (EllipseTool (Just p)) c
    PointerRelease p -> case t of
      PolygonTool _                     -> Model ss t c
      LineTool (Just firstPoint)        -> Model ((c,(Line firstPoint p)):ss) (LineTool Nothing) c
      RectangleTool (Just firstPoint)   -> Model ((c,(Rectangle firstPoint p)):ss) (RectangleTool Nothing) c
      CircleTool (Just firstPoint)      -> Model ((c,(Circle firstPoint p)):ss) (CircleTool Nothing) c
      EllipseTool (Just firstPoint)     -> Model ((c,(Ellipse firstPoint p)):ss) (EllipseTool Nothing) c
      _                                 -> Model ss t c
    _ -> m

-- | Returns the next colour from the set of ColourNames.
nextColour :: ColourName -> ColourName
nextColour colourName = case colourName of
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
  LineTool Nothing      -> PolygonTool []
  PolygonTool []        -> RectangleTool Nothing
  RectangleTool Nothing -> CircleTool Nothing
  CircleTool Nothing    -> EllipseTool Nothing
  EllipseTool Nothing   -> LineTool Nothing
  LineTool x            -> LineTool x
  PolygonTool x         -> PolygonTool x
  RectangleTool x       -> RectangleTool x
  CircleTool x          -> CircleTool x
  EllipseTool x         -> EllipseTool x