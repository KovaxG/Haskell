type Bird = Int
type Pole = (Bird, Bird)

landLeft :: Bird -> Pole -> Pole
landLeft newBirds (left, right)
	| abs (right - (left + newBirds)) < 4 = Just (left + newBirds, right)
	| otherwise = Nothing
	
landRight :: Bird -> Pole -> Pole
landRight newBirds (left, right)
	| abs (left - (right + newBirds)) < 4 = Just (left, right + newBirds)
	| otherwise = Nothing
	
banana :: Pole -> Pole
banana _ = Nothing

