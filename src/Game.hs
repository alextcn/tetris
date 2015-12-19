module Game (runGame) where

import Config
import World
import Drawing
import Figures

import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

runGame :: IO ()
runGame = play
        window       -- Display mode.
        background   -- Background color.
        fps          -- Number of simulation steps to take for each second of real time.
        initialState -- The initial world.
        render       -- A function to convert the world a picture.
        handler      -- A function to handle input events.
        update       -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.


window :: Display
window = InWindow "Tetris" (windowWidth, windowHeight) (20, 20)

background :: Color
background = (makeColorI 152 152 152 255)

-- | Frames per second.
--    Can be used for hardness.
fps :: Int
fps = 3

-- -- | Render function for game
render :: TetrisGame -> Picture
render game = fst $ runState (drawWindow $ fallingFigure game) defaultAppConfig

-- | A function to handle input events.
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