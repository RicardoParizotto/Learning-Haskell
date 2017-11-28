import CodeWorld


tree :: Integer -> Double -> Picture
tree 0 t = 
      if t < 10
      then colored yellow (solidCircle (t*0.02))
          else colored yellow (solidCircle (0.2))

tree n t = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) t) & rotated (- pi/10) (tree (n-1) t))
 
boom :: Double -> Picture
boom t = tree 8 t

main :: IO ()
main = animationOf boom
