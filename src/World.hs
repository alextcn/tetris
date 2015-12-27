{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module World where

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import System.Random
import Data.List
import Data.Ord

import Config
import Figures
import Util

type Grid = Map.Map GridPosition ()

oneLineScore = 100
twoLinesScore = 300
threeLinesScore = 700
fourLinesScore = 1500

data Hardness = Beginner | Learning | Average | Skilled | Masterful | Insane | Godlike
      deriving (Enum, Bounded, Show)

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
  , score           :: Integer
  , isPause         :: Bool
  , gameOver        :: Bool
  }
  deriving Show

-- | Initial state of the game
initialState :: ReaderT AppConfig IO TetrisGame
initialState = do
  cfg <- ask
  gen <- liftIO getStdGen
  let fs = randomFigures gen
  let startPos = startPosition cfg
  return $ Game (head fs) startPos startPos (fst $ gridSize $ cfg)
                (snd $ gridSize $ cfg) (tail fs) Map.empty Learning 0 False False

-- | Real position in Grid
getRealCoords :: Figure -> GridPosition -> [Block]
getRealCoords (Figure _ _ bs) curPos = map (sumPair curPos) bs

-- | List of random figures
randomFigures :: (RandomGen g) => g -> [Figure]
randomFigures gen = zipWith getFigures (randoms gen) (randoms gen)

-- | Sets the currently falling figure from nextFigures
nextFigureGame :: TetrisGame -> TetrisGame
nextFigureGame g@Game {..}
  | checkingGO = g { gameOver = True }
  | otherwise = updateHardness $ updateScore $ Game (head nextFigures) startFalling startFalling width height
                                              (tail nextFigures) updateGrid hardness score isPause checkingGO
                                              
  where
    updateScore gnew = gnew { score = score + (getScore $ countOfBurns g gnew) }
    
    
    countOfBurns (length . getGridAsList -> countOld) (length . getGridAsList -> countNew) = div ((countOld + 4) - countNew) width
    
    -- Returns score based on count of burned lines.
    getScore :: Int -> Integer
    getScore 0 = 0
    getScore 1 = 100
    getScore 2 = 300
    getScore 3 = 700
    getScore 4 = 1500
    
    updateHardness :: TetrisGame -> TetrisGame
    updateHardness = id -- TODO: implement calculating new hardness
    
    updateGrid = burnFullLines $ foldl (\gr bl -> (Map.insert bl () gr)) grid (getRealCoords fallingFigure fallingPosition)
    
    checkingGO = any (\(_,y) -> y >= height) (getRealCoords fallingFigure fallingPosition)
    
    burnFullLines = listToGrid
      . concat
      . zipWith (\num list -> map (\(x,y)->(x,num)) list) [0,1..]
      . filter ((/=width) . length)
      . groupBy (\(_,y1) (_,y2) -> y1 == y2)
      . sortBy (comparing snd)
      . gridToList


-- | Shifts left a figure if able to
shiftLeftFigure :: TetrisGame -> TetrisGame
shiftLeftFigure curTetrisGame@(Game ff (shiftLeft -> fpos) spos w h fs grid hrd scr isPs go)
  | goodCoords grid w h (getRealCoords ff fpos) = Game ff fpos spos w h fs grid hrd scr isPs go
  | otherwise = curTetrisGame


-- | Shifts right a figure if able to
shiftRightFigure :: TetrisGame -> TetrisGame
shiftRightFigure curTetrisGame@(Game ff (shiftRight -> fpos) spos w h fs grid hrd scr isPs go)
  | goodCoords grid w h (getRealCoords ff fpos) = Game ff fpos spos w h fs grid hrd scr isPs go
  | otherwise = curTetrisGame

-- | Shifts down a figure if able to
shiftDownFigure :: TetrisGame -> TetrisGame
shiftDownFigure curTetrisGame@(Game ff (shiftDown -> fpos) spos w h fs grid hrd scr isPs go)
  | goodCoords grid w h (getRealCoords ff fpos) = Game ff fpos spos w h fs grid hrd scr isPs go
  | otherwise = nextFigureGame curTetrisGame

-- | Rotates a figure if able to
rotateFigure :: TetrisGame -> TetrisGame
rotateFigure curTetrisGame@(Game (rotate -> ff) fpos spos w h fs grid hrd scr isPs go)
  | goodCoords grid w h (getRealCoords ff fpos) = Game ff fpos spos w h fs grid hrd scr isPs go
  | otherwise = curTetrisGame

resetGame :: TetrisGame -> TetrisGame
resetGame Game {..} = Game (head nextFigures) startFalling startFalling width height (tail nextFigures) Map.empty Learning 0 False False

-- | Checks that the point belongs to the Grid and that it is free
goodCoords :: Grid -> Int -> Int -> [Block] -> Bool
goodCoords grid w h = all goodCoord
  where
    goodCoord (x,y) = x >= 0 && x < w && y >= 0 && isNothing (Map.lookup (x,y) grid)

-- | Returns next figure
getNextFigure :: TetrisGame -> Figure
getNextFigure (nextFigures -> fs) = head fs

getGridAsList :: TetrisGame -> [GridPosition]
getGridAsList (grid -> gr) = gridToList gr

listToGrid :: [GridPosition] -> Grid
listToGrid = Map.fromList . (`zip` repeat ())

gridToList :: Grid -> [GridPosition]
gridToList = Map.keys

shiftRight :: GridPosition -> GridPosition
shiftRight = sumPair (1,0)

shiftLeft :: GridPosition -> GridPosition
shiftLeft = sumPair (-1,0)

shiftDown :: GridPosition -> GridPosition
shiftDown = sumPair (0,-1)