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

      | k == "Backspace" || k == "Delete" -> undefined -- TODO: drop the last added shape
      | k == " " -> undefined -- TODO: finish polygon vertices
      | k == "T" -> undefined -- TODO: switch tool
      | k == "C" -> undefined -- TODO: switch colour

      -- ignore other events
      | otherwise -> m
      where k = unpack key
    PointerPress p -> undefined -- TODO
    PointerRelease p -> undefined -- TODO
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