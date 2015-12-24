module Figures where

import Config
import qualified Data.Map as Map

-- Tetris figures

type Block = (Int, Int)
data FigureType = Cube | Tank
data Rotation = D1 | D2 | D3 | D4
  deriving (Enum)
data Figure = Figure FigureType Rotation [Block]

type AllFigures = Map.Map (FigureType, Rotation) Figure

initFigures :: AllFigures
initFigures = undefined