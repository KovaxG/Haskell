type Length = Double
type Angle = Double
data Vector3 = Vector3 Double Double Double deriving (Show)
type State = (Angle, Angle)


l1 :: Length
l1 = 1


l2 :: Length
l2 = 1


directKinematics :: State -> Vector3
directKinematics (q1, q2) = Vector3 x y z
    where x = 0
          y = l2 * cos (q1 + q2) + l1 * cos (q1)
          z = l2 * sin (q1 + q2) + l1 * sin (q1)


inverseKinematics :: Vector3 -> State
inverseKinematics (Vector3 x y z) = (q1, q2)
    where a = (y^2 + z^2 - l1^2 - l2^2) / (2 * l1 * l2)
          q2 = atan $ sqrt (1 - a^2) / a
          q1 = atan (z / y) - atan ((l2 * sin (q2)) / (l1 + l2 * cos (q2)))
          
          