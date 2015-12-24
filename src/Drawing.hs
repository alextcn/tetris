module Drawing (drawWindow) where

import Config
import Figures
import World
import Util

import Control.Monad.State
import Control.Monad.Reader

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- DRAWING FUNCTIONS

-- | Draws all debug components
drawDebug :: StateT TetrisGame (Reader AppConfig) Picture
drawDebug = return $ Pictures [drawCenterScreen, drawHelp]
  where
  drawCenterScreen = Color ( green) $ circleSolid 2
  drawHelp = translate (-250) 270 $ scale 0.2 0.2 $ text "Press P to pause or unpause the game."

-- | Draws a one basic block on the grid
drawBlock :: Block -> StateT TetrisGame (Reader AppConfig) Picture
drawBlock = undefined

-- | Draws a figure on the grid
drawFigure :: GridPosition -> Figure -> StateT TetrisGame (Reader AppConfig) Picture
drawFigure p (Figure _ _ bs) = mapM drawBlock (map (sumPair p) bs) >>=
                               (\ps -> return $ Pictures ps)

-- | Draws falling figure of the game state
drawGrid :: StateT TetrisGame (Reader AppConfig) Picture
drawGrid = undefined

-- | Draws a cup figures are falling into (with empty grid)
drawCup :: StateT TetrisGame (Reader AppConfig) Picture
drawCup = do
  -- TODO: drawEmptyGrid
  b <- drawBlock (1,1)
  (x, y) <- fmap cupPosition $ ask
  return $ Pictures []
  -- return $ Color black $ Graphics.Gloss.Line [ (x, y)
  --                       , (x, y - cupHeight)
  --                       , (x + cupWidth, y - cupHeight)
  --                       , (x + cupWidth, y) ]

-- | Draws right sidebar
drawSidebar :: StateT TetrisGame (Reader AppConfig) Picture
drawSidebar = return $ Pictures []

-- | Draws the left game window
drawGame :: StateT TetrisGame (Reader AppConfig) Picture
drawGame = do
  cupPic <- drawCup
  (x, y) <- fmap gamePosition $ ask
  -- let bordersPic = Color black
  --                  $ lineLoop [ (x, y)
  --                             , (x + gameWidth, y)
  --                             , (x + gameWidth, y - gameHeight)
  --                             , (x, y - gameHeight) ]
  return $ Pictures [ cupPic ]

-- | Draws the whole window picture
drawWindow :: StateT TetrisGame (Reader AppConfig) Picture
drawWindow = do
  gamePic <- drawGame
  sidebarPic <- drawSidebar
  debugPic <- drawDebug
  return $ Pictures [ gamePic, sidebarPic, debugPic ]
