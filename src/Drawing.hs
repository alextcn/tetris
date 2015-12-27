module Drawing (drawWindow) where

import Config
import Figures
import World
import Util

import Control.Monad.State
import Control.Monad.Reader
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- COLORS

toGlossColor Red = makeColorI 235 93 71 255
toGlossColor Yellow = makeColorI 230 220 37 255
toGlossColor Green = makeColorI 138 232 148 255
toGlossColor Blue = makeColorI 138 189 254 255
toGlossColor Violet = makeColorI 153 93 181 255

data Colors = Red | Yellow | Green | Blue | Violet
  deriving (Enum, Bounded)

-- Instance of class Random for Colors
instance Random Colors where
    random g = randomR (minBound, maxBound) g
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

-- CONSTANT COLORS FOR DRAWING FUNCTIONS

gridColor = makeColorI 200 200 200 255
gridBlockColor = makeColorI 5 5 5 196
blockColor = makeColorI 155 74 30 255
overlayColor = makeColorI 10 10 10 200

-- DRAWING FUNCTIONS

-- | Draws all help components
drawHelp :: StateT TetrisGame (Reader AppConfig) Picture
drawHelp = do
  state <- get
  conf <- ask
  let (x, y) = cupPosition conf
  let w = snd $ cupSize conf
  return $ Pictures $ writeInfo (x - 300, y + w) (-20) (hardness state) (score state)
    where
      writeInfo (x, y) step hard score = 
        snd $
        foldl (\(s, pics) str -> (s + step, pics ++ [translate x (y + s) $ scale 0.12 0.12 $ text str])) (step, [])
          [ "Press P to pause"
          , "or unpause the game."
          , "Press Up Arrow to rotate."
          , ""
          , "Hardness : " ++ (show hard)
          , "Score " ++ (show score) ]

-- | Draws a one basic block on the grid
drawBlock :: Block -> Color -> StateT TetrisGame (Reader AppConfig) Picture
drawBlock block color = do
  conf <- ask
  let cp = cupPosition conf
  let sz = blockSize conf
  let bxp = fst cp + ((fromIntegral $ fst block) * sz)
  let byp = snd cp + ((fromIntegral $ snd block) * sz)
  return $ Color color (polygon  [ (bxp + 1, byp + 1)
                                      , (bxp + sz - 1, byp + 1)
                                      , (bxp + sz - 1, byp + sz - 1)
                                      , (bxp + 1, byp + sz - 1) ] )

-- | Draws falling figure of the game state
drawFigure :: GridPosition -> Figure -> StateT TetrisGame (Reader AppConfig) Picture
drawFigure p f@(Figure _ _ bs) = 
  mapM (`drawBlock` gridBlockColor) (getRealCoords f p) >>= return . Pictures

-- | Draws a figure on the grid
drawGrid :: StateT TetrisGame (Reader AppConfig) Picture
drawGrid = do
  state <- get
  pics <- mapM (`drawBlock` gridBlockColor) (getGridAsList state)
  return $ Pictures pics

-- | Draws a cup figures are falling into (with empty grid)
drawCup :: StateT TetrisGame (Reader AppConfig) Picture
drawCup = do
  config <- ask
  (x, y) <- fmap cupPosition $ ask
  sz <- fmap cupSize $ ask
  let height = snd sz
  let width = fst sz
  return $ Pictures [drawEmptyGrid config, Line [ (x, y + height)
                                                , (x, y)
                                                , (x + width, y)
                                                , (x + width, y + height) ] ]

-- | Draws empty grid
drawEmptyGrid :: AppConfig -> Picture
drawEmptyGrid conf =
  let cp = cupPosition conf in
  let gsY = snd $ gridSize conf in  -- Number of cells along vertical axis
  let stepY = (snd $ cupSize conf) / fromIntegral gsY in  -- step y along vertical axis
  let gsX = fst $ gridSize conf in  -- Number of cells along horizontal axis
  let stepX = (fst $ cupSize conf) / fromIntegral gsX in  -- step x along horizontal axis
  Color gridColor $ Pictures [drawHorizontal cp gsY stepY, drawVertical cp gsX stepX]
    where
      drawHorizontal cp gsY stepY =
        let points = [snd cp + stepY * 1, snd cp + stepY * 2 .. snd cp + stepY * (fromIntegral gsY - 1)] in -- Y line coords along vertical axis
        Pictures $ zipWith (\p1 p2 -> Line [(fst cp, p1), (fst cp + (fst $ cupSize conf), p2)]) points points
      drawVertical cp gsX stepX =
        let points = [fst cp + stepX * 1, fst cp + stepX * 2 .. fst cp + (stepX * fromIntegral gsX - 1)] in -- X line coords along horizontal axis
        Pictures $ zipWith (\p1 p2 -> Line [(p1, snd cp), (p2, snd cp + (snd $ cupSize conf))]) points points

-- | Draws right sidebar
drawSidebar :: StateT TetrisGame (Reader AppConfig) Picture
drawSidebar = do
  state <- get
  conf <- ask
  pic <- drawNextFigure (gridSize conf) (getNextFigure state) (blockSize conf) (cupPosition conf)
  return $ Pictures [pic]
  where
    drawNextFigure pos fig bs cp =
      let np = (fst pos + 1, snd pos - 4) in
      drawFigure np fig >>= return
        . Pictures
        . ( : [ translate (fst cp + (fromIntegral $ fst np) * bs) (snd cp + (fromIntegral $ (snd np + 3)) * bs) 
              $ scale 0.15 0.15 
              $ text "Prepare for this" ] )

-- | Draws the left game window
drawGame :: StateT TetrisGame (Reader AppConfig) Picture
drawGame = do
  cupPic <- drawCup
  (x, y) <- fmap gamePosition $ ask
  return $ Pictures [ cupPic ]

drawGameOver :: StateT TetrisGame (Reader AppConfig) Picture
drawGameOver = do
  conf <- ask
  state <- get
  let (px, py) = cupPosition conf
  let (w, h) = cupSize conf
  let (_winw, _winh) = windowSize conf
  let (winw, winh) = (fromIntegral _winw, fromIntegral _winh)
  let (cx, cy) = (px + w / 2, py + h / 2)
  let overlay = Color overlayColor $ polygon [ (- winw, - winh)
                                                          , (- winw,   winh)
                                                          , (  winw,   winh)
                                                          , (  winw, - winh) ]
  return $ Pictures [ overlay
                    , Color red $ translate (cx - 72) cy $ scale 0.2 0.2 $ text "Game Over"
                    , Color white $ translate (cx - 72) (cy - 35) $ scale 0.15 0.15 $ text $ show $ score state ]

-- | Draws the whole window picture
drawWindow :: StateT TetrisGame (Reader AppConfig) Picture
drawWindow = do
  gamePic <- drawGame
  state <- get
  let pos = fallingPosition state
  let fig = fallingFigure state
  figurePic <- drawFigure pos fig
  sidebarPic <- drawSidebar
  helpPic <- drawHelp
  gridPic <- drawGrid
  gameOverOverlayPic <- drawGO $ gameOver state
  return $ Pictures [ gridPic, gamePic, figurePic, sidebarPic, helpPic, gameOverOverlayPic ]
    where
      drawGO True = drawGameOver
      drawGO False = return $ Pictures [] 