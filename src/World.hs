module World where

import Data.Array.IArray
import Control.Monad.Reader
import System.Random

import Config
import Figures

type Grid = Array GridPosition Bool

data Hardness = Easy | Medium | Hard

-- | Data represents the state of the Tetris game
data TetrisGame = Game
  { fallingFigure   :: Figure
  , fallingPosition :: GridPosition
  , width           :: Int
  , height          :: Int
  , nextFigures     :: [Figure]
  , grid            :: Grid
  , hardness        :: Hardness
  , isPause         :: Bool
  }

-- | Initial state of the game
initialState :: ReaderT AppConfig IO TetrisGame
initialState = do
  cfg <- ask
  gen <- liftIO getStdGen
  let fs = randomFigures gen
  let grid = initGrid (gridSize cfg)
  return $ Game (head fs) (startPosition cfg) 24 40 (tail fs) grid Easy False

-- | Initial grid state
initGrid :: GridSize -> Grid
initGrid (w, h) = listArray ((0,0), (w-1, h-1)) (repeat False)

-- | List of random figures
randomFigures :: (RandomGen g) => g -> [Figure]
randomFigures = undefined

-- | Sets the currently falling figure from nextFigure
nextFigureGame :: TetrisGame -> TetrisGame
nextFigureGame = undefined

-- | Shifts left a figure if able to
shiftLeftGame :: TetrisGame -> TetrisGame
shiftLeftGame = undefined

-- | Shifts right a figure if able to
shiftRightGame :: TetrisGame -> TetrisGame
shiftRightGame = undefined

-- | Shifts down a figure if able to
shiftDownGame :: TetrisGame -> TetrisGame
shiftDownGame = undefined