module Figures where

import Config

-- IMPLEMENTS FIGURES AND FUNCTIONS TO WORK WITH THEM

-- | 2D position on the screen.
type Position = (Float, Float)

-- | Width & Heigth of the block
type Size = Float

-- | Block is a unit of figure
data Block = Block Position Size    -- DO we need Size in here?
              deriving (Show)

-- | Figure is a basic element of the game
data Figure = Figure
            { blocks :: [Block]
            } deriving (Show)

-- | Constructor for a cube figure
cube :: Figure
cube
  = Figure [ Block (0, 0) blockSize
           , Block (blockSize, 0) blockSize
           , Block (blockSize, blockSize) blockSize
           , Block (0, blockSize) blockSize ]

-- | Shifts figure one block left
shiftLeft :: Figure -> Figure
shiftLeft (Figure bs)
  = Figure (map (\z -> deposeBlock z (-blockSize) 0) bs)

-- | Shifts figure one block right
shiftRight :: Figure -> Figure
shiftRight (Figure bs)
  = Figure (map (\z -> deposeBlock z (blockSize) 0) bs) 

-- | Shifts figure one block down (used for falling)
shiftDown :: Figure -> Figure
shiftDown (Figure bs)
  = Figure (map (\z -> deposeBlock z 0 (-blockSize)) bs) 

-- | Deposes one particular block 
deposeBlock :: Block -> Float -> Float -> Block
deposeBlock (Block (x, y) sz) offX offY 
  = Block (x + offX, y + offY) sz

-- | Retunrs True when figure reaches the bommom
--    of the cup
fell :: Figure -> Bool
fell (Figure bs) = foldl _fell False bs
  where
    _fell True _ = True
    _fell False (Block (_, y) _) = y <= bottom
    (x, bottom) = cupBottomLeft

-- | Returns True when figure is near left border
blockedFromLeft :: Figure -> Bool
blockedFromLeft (Figure bs) = False

-- | Returns True when figure is near right border
blockedFromRight :: Figure -> Bool
blockedFromRight (Figure bs) = False