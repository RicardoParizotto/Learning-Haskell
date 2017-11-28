{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

botCircle, topCircle, midCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
topCircle c = colored c (translated 0   3  (solidCircle 1))
midCircle c = colored c (translated 0   0  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 8.5

trafficLight :: Integer -> Picture
trafficLight 0  = botCircle green & midCircle black & topCircle black & frame
trafficLight 1 = botCircle black & midCircle yellow & topCircle black & frame
trafficLight 2= botCircle black & midCircle black & topCircle red & frame
trafficLight 3 = botCircle black & midCircle yellow & topCircle red & frame


time :: Double -> Integer
time t = round(t) `mod` 10


trafficController :: Double -> Picture
trafficController t = 
        if time t < 5 && time t > 0
             then trafficLight 0 
             else if time t == 5
                 then trafficLight 1
                 else if time t > 5
                     then trafficLight 2
                     else trafficLight 3
                                                  
main :: IO ()
main = animationOf trafficController
