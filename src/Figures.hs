module Figures where

import Config
import qualified Data.Map as Map

-- Tetris figures

type Block = (Int, Int)
data FigureType = Cube | Tank | Stick | Snake1 | Snake2 | Corner1 | Corner2
data Rotation = D1 | D2 | D3 | D4
  deriving (Enum)

nextRotation :: Rotation -> Rotation
nextRotation D1 = D2
nextRotation D2 = D3
nextRotation D3 = D4
nextRotation D4 = D1
  
data Figure = Figure FigureType Rotation [Block]

rotate :: Figure -> Figure
rotate (Figure t r _) = allFigures t (nextRotation r)

allFigures :: FigureType -> Rotation -> Figure
allFigures Cube r = Figure Cube r [(0,0),(0,1),(1,1),(1,0)]

allFigures Tank D1 = Figure Tank D1 [(0,0),(0,1),(-1,0),(1,0)]
allFigures Tank D2 = Figure Tank D2 [(0,0),(0,1),(0,-1),(1,0)]
allFigures Tank D3 = Figure Tank D3 [(0,0),(0,-1),(-1,0),(1,0)]
allFigures Tank D4 = Figure Tank D4 [(0,0),(0,1),(0,-1),(-1,0)]

allFigures Stick D1 = Figure Stick D1 [(0,0),(-1,0),(1,0),(2,0)]
allFigures Stick D2 = Figure Stick D2 [(0,0),(0,-1),(0,-2),(0,1)]
allFigures Stick D3 = Figure Stick D3 [(0,0),(-1,0),(1,0),(2,0)]
allFigures Stick D4 = Figure Stick D4 [(0,0),(0,-1),(0,-2),(0,1)]
    
allFigures Snake1 D1 = Figure Snake1 D1 [(0,0),(-1,0),(0,-1),(1,-1)]
allFigures Snake1 D2 = Figure Snake1 D2 [(0,0),(0,-1),(1,0),(1,1)]
allFigures Snake1 D3 = Figure Snake1 D3 [(0,0),(-1,0),(0,-1),(1,-1)]
allFigures Snake1 D4 = Figure Snake1 D4 [(0,0),(0,-1),(1,0),(1,1)]    

allFigures Snake2 D1 = Figure Snake2 D1 [(0,0),(1,0),(0,-1),(-1,-1)]
allFigures Snake2 D2 = Figure Snake2 D2 [(0,0),(0,-1),(-1,0),(-1,1)]
allFigures Snake2 D3 = Figure Snake2 D3 [(0,0),(1,0),(0,-1),(-1,-1)]
allFigures Snake2 D4 = Figure Snake2 D4 [(0,0),(0,-1),(-1,0),(-1,1)]

allFigures Corner1 D1 = Figure Corner1 D1 [(0,0),(1,0),(-1,0),(-1,1)]
allFigures Corner1 D2 = Figure Corner1 D2 [(0,0),(0,-1),(0,1),(1,1)]
allFigures Corner1 D3 = Figure Corner1 D3 [(0,0),(1,0),(1,-1),(-1,0)]
allFigures Corner1 D4 = Figure Corner1 D4 [(0,0),(0,-1),(-1,-1),(0,1)]

allFigures Corner2 D1 = Figure Corner2 D1 [(0,0),(-1,0),(1,0),(1,1)]
allFigures Corner2 D2 = Figure Corner2 D2 [(0,0),(0,-1),(1,-1),(0,1)]
allFigures Corner2 D3 = Figure Corner2 D3 [(0,0),(-1,0),(-1,-1),(1,0)]
allFigures Corner2 D4 = Figure Corner2 D4 [(0,0),(0,-1),(0,1),(-1,1)]