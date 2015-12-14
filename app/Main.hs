  
import Figures
import Config
import World

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "Tetris" (windowWidth, windowHeight) (20,  20)

background :: Color
background = (makeColorI 152 152 152 255)

help :: Picture
help = translate (-250) 270 $ scale 0.2 0.2 $ text "Press P to pause or unpause the game."

-- | Frames per second.
--    Can be used for hardness.
fps :: Int
fps = 3

-- | Render function for game
render :: TetrisGame -> Picture
render game = Pictures [ figure
                       , field
                       , cup
                       -- for Debug
                       , centerScreen
                       , help ]
  where
    figure = drawFigure $ fallingFigure game
    field = drawGame
    cup = drawCup

handler :: Event -> TetrisGame -> TetrisGame

-- | Handles "pause" button
handler (EventKey (Char 'p') Down _ _) game 
  = game { isPause = not $ isPause game }

-- | Handles "left" button
handler (EventKey (SpecialKey KeyLeft) Down _ _) game 
  = shiftLeftGame game

-- | Handles "right" button
handler (EventKey (SpecialKey KeyRight) Down _ _) game 
  = shiftRightGame game

-- | Handles the rest input
handler _ game = game

-- | Updates game state by shifting current falling figure down
update :: Float -> TetrisGame -> TetrisGame
update _ game 
  | isPause game = game
  | fell $ fallingFigure game = nextFigureGame game
  | otherwise = shiftDownGame game

main
  = play window background fps initialState render handler update


----------  Some draw functions. Need to be relocated. ----------

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

-- | Draws a cup figures are falling into
drawCup :: Picture
drawCup 
    = let (x, y) = cupPosition in
    Color black 
    $ Graphics.Gloss.Line [ (x, y)
                          , (x, y - cupHeight)
                          , (x + cupWidth, y - cupHeight)
                          , (x + cupWidth, y) ]

-- | Draws game field (i.e. game window)
drawGame :: Picture
drawGame 
    = let (x, y) = gamePosition in
    Color black 
    $ lineLoop [ (x, y)
               , (x + gameWidth, y)
               , (x + gameWidth, y - gameHeight)
               , (x, y - gameHeight) ]

-- | Draws a single dot in the center of the screen.
--    Implemented for debug purposes.
centerScreen :: Picture
centerScreen 
  = Color ( green) $ circleSolid 2