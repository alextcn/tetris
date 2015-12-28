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
  let cfg = defaultAppConfig
  game <- initState cfg
  play
    (window cfg)   -- Display mode.
    background     -- Background color.
    fps            -- Number of simulation steps to take for each second of real time.
    game           -- The initial world.
    (render cfg)   -- A function to convert the world a picture.
    handler        -- A function to handle input events.
    update         -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.

window :: AppConfig -> Display
window cfg = InWindow "Tetris" (windowSize cfg) (windowPosition cfg)

-- | Smallest common multiple
fps = 24

-- | Hardness fraps per second * (1/fps)
hardnessMod :: Hardness -> Integer
hardnessMod Beginner = 24
hardnessMod Average = 12
hardnessMod Skilled = 6
hardnessMod Masterful = 4
hardnessMod Insane = 3
hardnessMod Godlike = 2


background :: Color
background = (makeColorI 29 31 33 255)

initState :: AppConfig -> IO TetrisGame
initState = runReaderT (initialState)

-- | Render function for game
render :: AppConfig -> TetrisGame -> Picture
render cfg game = runReader (evalStateT drawWindow game) cfg

-- | Updates game state by shifting current falling figure down
update :: Float -> TetrisGame -> TetrisGame
update _ game = case (isPause game, gameOver game) of
                  (False, False) -> case (needUpdate) of
                                     True -> shiftDownFigure (game { frapsCounter = 0 })
                                     False -> game { frapsCounter = (frapsCounter game) + 1}
                  (_, True) -> game { isPause = True}
                  (True, _) -> game

  where
    needUpdate = (==0) $ mod ((+1) $ frapsCounter game) (hardnessMod $ hardness game)

-- | A function to handle input events.
handler :: Event -> TetrisGame -> TetrisGame

-- | Handles "pause" button
handler (EventKey (Char 'p') Down _ _) game
  = game { isPause = not $ isPause game }

-- | Handles "reset" button
handler (EventKey (Char 'r') Down _ _) game
  = resetGame game

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