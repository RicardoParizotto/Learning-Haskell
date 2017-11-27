{-# LANGUAGE OverloadedStrings #-}
import CodeWorld


topPicture color = colored color (translated 0 (1.5) ( solidCircle 1 ))
botPicture color = colored color (translated 0 (-1.5) (solidCircle 1))
frame = rectangle 2.5 5.5

ourPicture :: Picture

trafficLight :: Bool -> Picture

trafficLight n = if n == True then topPicture red & botPicture green & frame else topPicture green & botPicture red & frame

{-
trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 2 == 0 = trafficLight True
  | otherwise                = trafficLight False
  
-}

lights :: Integer -> Picture
lights 0 = blank
lights n = trafficLight True & translated 3 0 (lights (n-1))

ourPicture = lights 3

main = drawingOf ourPicture
