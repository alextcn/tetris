module World where

import Config
import Figures

-- | Data represents the state of the Tetris game
data TetrisGame = Game
  { fallingFigure :: Figure
  , nextFigure    :: Figure
  , falledBlocks  :: [Block]
  , isPause       :: Bool
  } deriving (Show)


-- | Initial state of the game
initialState :: TetrisGame
initialState
  = Game (toStart cube) (toStart cube) [] True

-- | Sets the currently falling figure from nextFigure
nextFigureGame :: TetrisGame -> TetrisGame
nextFigureGame (Game cur nxt bs pause) = Game nxt (toStart cube) bs pause

-- | Shifts left a figure if able to
shiftLeftGame :: TetrisGame -> TetrisGame
shiftLeftGame game
  | isPause game = game
  | blockedFromLeft $ fallingFigure game = game
  | otherwise = game { fallingFigure = shiftLeft $ fallingFigure game }

-- | Shifts right a figure if able to
shiftRightGame :: TetrisGame -> TetrisGame
shiftRightGame game
  | isPause game = game
  | blockedFromRight $ fallingFigure game = game
  | otherwise = game { fallingFigure = shiftRight $ fallingFigure game }

-- | Shifts down a figure if able to
shiftDownGame :: TetrisGame -> TetrisGame
shiftDownGame game
  | isPause game = game
  | fell $ fallingFigure game = game
  | otherwise = game { fallingFigure = shiftDown $ fallingFigure game }