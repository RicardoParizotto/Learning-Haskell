{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

wall, ground, box, storage :: Picture
wall = colored green (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
box = colored brown (solidRectangle 1 1 )
storage = colored black (solidRectangle 1 1)


drawTile :: Integer -> Picture
drawTile x =
        case x of
            1 -> wall
            2 -> ground
            3 -> storage
            _ -> box
                     

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
  

drawMaze :: Integer -> Integer -> Picture
drawMaze 4 y = drawTile (maze 4 y)
drawMaze x (-4) = drawTile (maze x (-4))
drawMaze x y = drawTile (maze x y) & translated 0 (-1) (drawMaze x (y-1)) & translated 1 0 (drawMaze (x+1) (y))

pictureOfMaze :: Picture
pictureOfMaze = translated (-4) 4 (drawMaze (-4) 4)

main :: IO ()
main = drawingOf pictureOfMaze


