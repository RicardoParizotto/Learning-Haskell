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
  
  


drawMaze :: 

drawTile (maze x y) 


pictureOfMaze :: Picture


{-
drawTile :: Integer -> Picture
-}





main :: IO ()
main = drawingOf wall


