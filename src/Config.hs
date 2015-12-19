module Config where

-- ALL THE GLOBAL PARAMS STACKED IN HERE

------------------ Size ------------------

-- | Width of the window
windowWidth :: Int
windowWidth               = 1024

-- | Height of the window
windowHeight :: Int
windowHeight              = 600

-- | Width of the game field
gameWidth :: Float
gameWidth                 = 500

-- | Height of the game field
gameHeight :: Float
gameHeight                = 500

-- | Width of the cup
cupWidth :: Float
cupWidth                  = 24 * blockSize

-- | Height of the cup
cupHeight :: Float
cupHeight                 = 40 * blockSize

-- | Default size of the all blocks
blockSize :: Float
blockSize                 = 10

------------------ Position ------------------

-- | Window position relates to screen
windowPosition :: (Int, Int)
windowPosition            = (20, 20)

-- | Game position relates to window
gamePosition :: (Float, Float)
gamePosition              = (-gameWidth / 2, gameHeight / 2)

-- | Cup position relates to game
cupPosition :: (Float, Float)
cupPosition               = let (x,y) = gamePosition in
                            (x + gameWidth * 0.1, y - gameHeight * 0.1)

-- | Cup left-bottom corner
cupBottomLeft :: (Float, Float)
cupBottomLeft             = let (x,y) = cupPosition in
                            (x, y - cupHeight)

-- | Cup right-bottom corner
cupBottomRight :: (Float, Float)
cupBottomRight            = let (x,y) = cupPosition in
                            (x + cupWidth, y - cupHeight)

-- | Starting position of the falling figures (is it global?)
startPosition :: (Float, Float)
startPosition             = (x + cupWidth / 2, y + blockSize * 2)
                            where
                              (x, y) = cupPosition