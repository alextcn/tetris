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
  game <- initState
  play
    window         -- Display mode.
    background     -- Background color.
    (calcFps game) -- Number of simulation steps to take for each second of real time.
    game           -- The initial world.
    render         -- A function to convert the world a picture.
    handler        -- A function to handle input events.
    update         -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.


window :: Display
window = InWindow "Tetris" defaultWindowSize defaultWindowPosition

background :: Color
background = (makeColorI 170 170 170 255)

-- | Frames per second.
--    Can be used for hardness.
calcFps :: TetrisGame -> Int
calcFps game = (+3) . fromEnum $ hardness game

initState :: IO TetrisGame
initState = runReaderT (initialState) defaultAppConfig

-- | Render function for game
render :: TetrisGame -> Picture
render game = runReader (evalStateT drawWindow game) defaultAppConfig

-- | Updates game state by shifting current falling figure down
update :: Float -> TetrisGame -> TetrisGame
update _ game = case (isPause game, gameOver game) of
                  (False, False) -> shiftDownFigure game
                  (_, True) -> game { isPause = True}
                  (True, _) -> game

-- | A function to handle input events.
handler :: Event -> TetrisGame -> TetrisGame

-- | Handles "pause" button
handler (EventKey (Char 'p') Down _ _) game
  = game { isPause = not $ isPause game }

-- | Handles "reset" button
handler (EventKey (Char 'r') Down _ _) _
  = initState

-- | Handles "left" button
handler (EventKey (SpecialKey KeyLeft) Down _ _) game
  = case isPause game of
      False -> shiftLeftFigure game
      True -> game

-- | Handles "right" button
handler (EventKey (SpecialKey KeyRight) Down _ _) game
  = case isPause game of
      False -> shiftRightFigure game
      True -> game

-- | Handles "up" button
handler (EventKey (SpecialKey KeyUp) Down _ _) game
  = case isPause game of
      False -> rotateFigure game
      True -> game

-- | Handles "up" button
handler (EventKey (SpecialKey KeyDown) Down _ _) game
  = case isPause game of
      False -> shiftDownFigure game
      True -> game

-- | Handles "esc" button
handler (EventKey (SpecialKey KeyEsc) Down _ _) game
  = error "close game" -- wow!


-- | Handles the rest input
handler _ game = game