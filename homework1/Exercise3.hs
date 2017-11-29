{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

wall, ground, box, storage :: Picture
wall = colored green (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
box = colored brown (solidRectangle 1 1 )
storage =  colored black (solidCircle 0.1) & ground


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
  
  

drawColumn :: Integer -> Integer -> Picture
drawColumn 11 _ = blank
drawColumn x y = drawTile (maze x y) & translated 1 0 ( drawColumn (x+1) y)


drawLine :: Integer -> Picture
drawLine 11 = blank
drawLine y = translated 0 (-1) (( drawColumn 0 y ) & drawLine (y+1))


drawMaze :: Picture
drawMaze = translated (-5) 5 (drawLine (0))

pictureOfMaze :: Picture
pictureOfMaze = drawMaze 


main :: IO ()
main = drawingOf pictureOfMaze


