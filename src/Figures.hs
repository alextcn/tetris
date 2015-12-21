module Figures where

import Config
import qualified Data.Map as Map

-- IMPLEMENTS FIGURES AND FUNCTIONS TO WORK WITH THEM

-- | 2D position on the screen.
type Position = (Int, Int)

-- | Width & Heigth of the block
type Size = Float

type Block = (Int, Int)
data FigureType = Cube | Tank
data Rotation = Up1 | Right1 | Down1 | Left1
data Figure = Figure FigureType [Block] Rotation

type AllFigures = Map.Map (FigureType, Rotation) Figure

initFigures :: AllFigures
initFigures = undefined