module Main where

import Controller
import Model
import View
import Testing

-- | The list of all tests to run.
tests :: [Test]
tests = toolLabelTests ++ nextColourTests ++ nextToolTests

toolLabelTests :: [Test]
toolLabelTests =
  [ Test "LineTool"
      (assertEqual (toolLabel (LineTool Nothing))
       "Line... click-drag-release")
  , Test "PolygonTool"
      (assertEqual (toolLabel (PolygonTool []))
      "Polygon... click 3 or more times then spacebar")
  , Test "RectangleTool"
      (assertEqual (toolLabel (RectangleTool Nothing))
      "Rectangle... click-drag-release")
  , Test "CircleTool"
      (assertEqual (toolLabel (CircleTool Nothing))
      "Circle... click-drag-release")
  , Test "EllipseTool"
      (assertEqual (toolLabel (EllipseTool Nothing))
      "Ellipse... click-drag-release")
  ]

nextColourTests :: [Test]
nextColourTests =
  [ Test "Black -> Red" (assertEqual (nextColour Black) Red)
  , Test "Red -> Orange" (assertEqual (nextColour Red) Orange)
  , Test "Orange -> Yellow" (assertEqual (nextColour Orange) Yellow)
  , Test "Yellow -> Green" (assertEqual (nextColour Yellow) Green)
  , Test "Green -> Blue" (assertEqual (nextColour Green) Blue)
  , Test "Blue -> Violet" (assertEqual (nextColour Blue) Violet)
  , Test "Violet -> Black" (assertEqual (nextColour Violet) Black)
  ]

-- | Tests for nextTool, including tests that it doesn't cycle tools
-- midway through a draw.
nextToolTests :: [Test]
nextToolTests =
  [ Test "Line -> Polygon"
      (assertEqual (nextTool (LineTool Nothing)) (PolygonTool []))
  , Test "Polygon -> Rectangle"
      (assertEqual (nextTool (PolygonTool [])) (RectangleTool Nothing))
  , Test "Rectangle -> Circle"
      (assertEqual (nextTool (RectangleTool Nothing)) (CircleTool Nothing))
  , Test "Circle -> Ellipse"
      (assertEqual (nextTool (CircleTool Nothing)) (EllipseTool Nothing))
  , Test "Ellipse -> Line"
      (assertEqual (nextTool (EllipseTool Nothing)) (LineTool Nothing))
  , Test "Line (in use) -> Line"
      (assertEqual (nextTool (LineTool (Just (1,1)))) (LineTool (Just (1,1))))
  , Test "Polygon (in use) -> Polygon"
      (assertEqual (nextTool (PolygonTool [(1,1)])) (PolygonTool [(1,1)]))
  , Test "Rectangle (in use) -> Rectangle"
      (assertEqual (nextTool (RectangleTool (Just (1,1)))) (RectangleTool (Just (1,1))))
  , Test "Circle (in use) -> Circle"
      (assertEqual (nextTool (CircleTool (Just (1,1)))) (CircleTool (Just (1,1))))
  , Test "Ellipse (in use) -> Ellipse"
      (assertEqual (nextTool (EllipseTool (Just (1,1)))) (EllipseTool (Just (1,1))))
  ]

-- | A haskell program starts by running the computation defined by
-- 'main'. We run the list of tests that we defined above.
main :: IO ()
main = runTests tests
