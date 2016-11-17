{-
 - RCS module
 -}
 
 module RCS (
 Axis,
 trans,  
 rot, 
 point,
 p,
 t,
 r
 ) where
 
 import Data.Matrix
 
 data Axis = X | Y | Z deriving Show
 
 -- Only works for 3D
 trans :: Axis -> Double -> Matrix Double
 trans X x = fromLists [[1,0,0,x], [0,1,0,0], [0,0,1,0], [0,0,0,1]]
 trans Y y = fromLists [[1,0,0,0], [0,1,0,y], [0,0,1,0], [0,0,0,1]]
 trans Z z = fromLists [[1,0,0,0], [0,1,0,0], [0,0,1,z], [0,0,0,1]]
 
 -- Only works for 3D
 rot :: Axis -> Double -> Matrix Double
 rot X a = fromLists [[1,0,0,0], [0,cos a, -sin a,0], [0,sin a,cos a,0], [0,0,0,1]]
 rot Y b = fromLists [[cos b,0,sin b,0], [0,1,0,0], [ -sin b,0,cos b,0], [0,0,0,1]]
 rot Z c = fromLists [[cos c,-sin c,0,0], [sin c, cos c,0,0], [0,0,1,0], [0,0,0,1]]
 
 -- Create a 3D point
 point :: (Double, Double, Double) -> Matrix Double
 point (a,b,c) = fromList 4 1 [a, b, c, 1]
 
 -- Shorthand
 p = point 
 t = trans
 r = rot
 
