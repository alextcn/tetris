module Game (runGame) where

import Config
import World
import Drawing
import Figures

import Control.Monad.State
import Control.Monad.Reader
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

runGame :: IO ()
runGame = do
  state <- initState
  play
        window       -- Display mode.
        background   -- Background color.
        fps          -- Number of simulation steps to take for each second of real time.
        state        -- The initial world.
        render       -- A function to convert the world a picture.
        handler      -- A function to handle input events.
        update       -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.


window :: Display
window = InWindow "Tetris" defaultWindowSize defaultWindowPosition

background :: Color
background = (makeColorI 170 170 170 255)

-- | Frames per second.
--    Can be used for hardness.
fps :: Int
fps = 3

initState :: IO TetrisGame
initState = runReaderT (initialState) defaultAppConfig

-- | Render function for game
render :: TetrisGame -> Picture
render game = runReader (evalStateT drawWindow game) defaultAppConfig

-- | Updates game state by shifting current falling figure down
update :: Float -> TetrisGame -> TetrisGame
update _ game = case isPause game of
                  False -> shiftDownFigure game
                  True -> game

-- | A function to handle input events.
handler :: Event -> TetrisGame -> TetrisGame

-- | Handles "pause" button
handler (EventKey (Char 'p') Down _ _) game
  = game { isPause = not $ isPause game }

-- | Handles "left" button
handler (EventKey (SpecialKey KeyLeft) Down _ _) game
  = shiftLeftFigure game

-- | Handles "right" button
handler (EventKey (SpecialKey KeyRight) Down _ _) game
  = shiftRightFigure game

-- | Handles "up" button
handler (EventKey (SpecialKey KeyUp) Down _ _) game
  = rotateFigure game

-- | Handles "up" button
handler (EventKey (SpecialKey KeyDown) Down _ _) game
  = shiftDownFigure game


-- | Handles the rest input
handler _ game = game