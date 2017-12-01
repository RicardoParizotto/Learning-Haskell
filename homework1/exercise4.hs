{-# LANGUAGE OverloadedStrings #-}
{- most of this code can be found in https://www.seas.upenn.edu/%7Ecis194/fall16/lectures/02-ho-datatypes.html -}

import CodeWorld

wall, ground, box, storage, player :: Picture
wall = colored green (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
box = colored brown (solidRectangle 1 1 )
storage =  colored black (solidCircle 0.1) & ground
player =  colored brown (path [(0,0),(0,0.2)] & path [(0,0),(0.2, 0.2)] & path [(0,0),((-0.2),0.2)]) & translated 0 0.2 (solidCircle 0.1) & path [(0,0),(0,(-0.2))] & ground

data Tile = Wall | Ground | Storage | Box | Player
data Coord = C Integer Integer
data Direction = R | U | L | D

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze r c))

drawTile :: Tile -> Picture
drawTile x =
        case x of
            Wall -> wall
            Ground -> ground
            Storage -> storage
            Box -> box
            Player -> player
                       


maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 4  || abs y > 4  = Box
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
  
draw21Tiles :: ( Integer -> Picture ) -> Picture
draw21Tiles something = go (-4)
    where 
      go :: Integer -> Picture
      go 5 = blank
      go n = something n & go (n+1)
      
pictureOfMaze :: Picture
pictureOfMaze = draw21Tiles (\r -> draw21Tiles (\c -> (drawTileAt r c)))

initialCoord :: Coord
initialCoord = C (-2) (-2)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawPlayer :: Coord -> Picture
drawPlayer c = atCoord c (drawTile Player)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up"    = adjacentCoord U c
    | key == "Left"  = adjacentCoord L c
    | key == "Down"  = adjacentCoord D c
handleEvent _ c      = c

  
main = interactionOf initialCoord handleTime handleEvent drawState

handleTime :: Double -> Coord -> Coord
handleTime _ c = c


drawState :: Coord -> Picture
drawState c = drawPlayer c & pictureOfMaze 


