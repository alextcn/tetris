module Drawing (drawWindow) where

import Config
import Figures

import Control.Monad.State

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- DRAWING FUNCTIONS

-- | Draws a one basic block and its contour
drawBlock :: Block -> Picture
drawBlock (Block (x,y) size)
    = Pictures [ Color (makeColorI 200 200 200 255)
                 $ Polygon [ (x, y)
                           , (x + size, y)
                           , (x + size, y + size)
                           , (x , y + size) ]
               , Color black
                 $ lineLoop [ (x, y)
                            , (x + size, y)
                            , (x + size, y + size)
                            , (x , y + size) ] ]

-- | Draws entire figure
drawFigure :: Figure -> Picture
drawFigure (Figure blocks)
        = Pictures $ map (\x -> drawBlock x) blocks

-- drawFigure :: Position -> Figure -> State AppConfig Picture
-- drawFigure = undefined

-- | Draws a single dot in the center of the screen.
--    Implemented for debug purposes.
drawCenterScreen :: Picture
drawCenterScreen
  = Color ( green) $ circleSolid 2

drawHelp :: Picture
drawHelp = translate (-250) 270 $ scale 0.2 0.2 $ text "Press P to pause or unpause the game."

-- | Draws a cup figures are falling into
drawCup :: State AppConfig Picture
drawCup = do
  (x, y) <- fmap _cupPosition $ get
  return $ Color black $ Graphics.Gloss.Line [ (x, y)
                        , (x, y - cupHeight)
                        , (x + cupWidth, y - cupHeight)
                        , (x + cupWidth, y) ]

-- | Draws right sidebar
drawSidebar :: State AppConfig Picture
drawSidebar = return $ Pictures []

-- | Draws the left game window
drawGame :: State AppConfig Picture
drawGame = do
  cupPic <- drawCup
  (x, y) <- fmap _gamePosition $ get
  let bordersPic = Color black
                   $ lineLoop [ (x, y)
                              , (x + gameWidth, y)
                              , (x + gameWidth, y - gameHeight)
                              , (x, y - gameHeight) ]
  return $ Pictures [ cupPic, bordersPic ]

-- | Draws the whole window picture
drawWindow :: Figure -> State AppConfig Picture
drawWindow fallingFugure = do
  let figPic = drawFigure fallingFugure
  gamePic <- drawGame
  sidebarPic <- drawSidebar
  return $ Pictures [ figPic, drawHelp, gamePic, sidebarPic, drawCenterScreen ]
