{-# LANGUAGE OverloadedStrings #-}
import CodeWorld


midPicture color = colored color (translated 0 (0) (solidCircle 1))
botPicture color = colored color (translated 0 (-3) (solidCircle 1))
topPicture color = colored color (translated 0 3 (solidCircle 1))

frame = rectangle 2.5 8.5


trafficLight :: Integer -> Picture

trafficLight 1 = topPicture green & midPicture black & botPicture black & frame
trafficLight 2 = topPicture black & midPicture yellow & botPicture black & frame
trafficLight 3 = topPicture black & midPicture black & botPicture red & frame
trafficLight 4 = topPicture black & midPicture yellow & botPicture red & frame


trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 2 == 0 = trafficLight 1
  | otherwise                = trafficLight 2


main :: IO ()
main = animationOf trafficController
