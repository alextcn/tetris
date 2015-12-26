{-# LANGUAGE ViewPatterns #-}
module World where

import Data.Array.IArray
import Control.Monad.Reader
import System.Random

import Config
import Figures
import Util

type Grid = Array GridPosition Bool

data Hardness = Easy | Medium | Hard

-- | Data represents the state of the Tetris game
data TetrisGame = Game
  { fallingFigure   :: Figure
  , fallingPosition :: GridPosition
  , startFalling    :: GridPosition
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
  let startPos = startPosition cfg
  return $ Game (head fs) startPos startPos 24 40 (tail fs) grid Easy False

-- | Real position in Grid
getRealCoords :: Figure -> GridPosition -> [Block]
getRealCoords (Figure _ _ bs) curPos = map (sumPair curPos) bs
  
-- | Initial grid state
initGrid :: GridSize -> Grid
initGrid (w, h) = listArray ((0,0), (w-1, h-1)) (repeat False)

-- | List of random figures
randomFigures :: (RandomGen g) => g -> [Figure]
randomFigures gen = zipWith getFigures (randoms gen) (randoms gen)

-- | Sets the currently falling figure from nextFigures
nextFigureGame :: TetrisGame -> TetrisGame
nextFigureGame (Game ff fpos spos w h fs grid hrd isPs) = 
                          Game (head fs) spos spos w h (tail fs) grid hrd isPs

-- | Shifts left a figure if able to
shiftLeftGame :: TetrisGame -> TetrisGame
shiftLeftGame curTetrisGame@(Game ff ((sumPair (-1,0)) -> fpos) spos w h fs grid hrd isPs)
  | goodCoords (getRealCoords ff fpos) grid w h = curTetrisGame
  | otherwise = Game ff fpos spos w h fs grid hrd isPs

-- | Shifts right a figure if able to
shiftRightGame :: TetrisGame -> TetrisGame
shiftRightGame curTetrisGame@(Game ff ((sumPair (1,0)) -> fpos) spos w h fs grid hrd isPs)
  | goodCoords (getRealCoords ff fpos) grid w h = curTetrisGame
  | otherwise = Game ff fpos spos w h fs grid hrd isPs

-- | Shifts down a figure if able to
shiftDownGame :: TetrisGame -> TetrisGame
shiftDownGame curTetrisGame@(Game ff ((sumPair (0,-1)) -> fpos) spos w h fs grid hrd isPs)
  | goodCoords (getRealCoords ff fpos) grid w h = curTetrisGame
  | otherwise = Game ff fpos spos w h fs grid hrd isPs

goodCoords :: [Block] -> Grid -> Int -> Int -> Bool
goodCoords bs grid w h = all (goodCoord grid w h) bs
  where
    goodCoord :: Grid -> Int -> Int -> Block -> Bool
    goodCoord gr w h (x,y) = x >= 0 && x < w && y >= 0 && y < h && not (grid ! (x,y))