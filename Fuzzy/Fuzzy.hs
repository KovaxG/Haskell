{- 
 - Haskell Fuzzy Logic Implementation
 - for Distributed Control Systems
 - Date: 2016.11.04
 - by Kovacs Gyorgy
 -}

 -- The FuzzyMember Class, you can easily define new types of fuzzy logic,
 -- by implementing it as member of this class, i.e. implement each functions
 -- and you are set.
class FuzzySymbol a where
    weightedFuzzySet :: [(a, Double)] -- A sorted list of symbols of the fuzzySets and their respective weights 
    fuzzySymbols :: [a] -- A sorted list of symbols of the fuzzy set (you don't have to implement it)
    fuzzySymbols = fst <$> weightedFuzzySet
    membership ::  Double -> a -> Double -- How much is this value part of a given symbol
    toFuzzy :: [a] -> Double -> [(a, Double)] -- Convert a double to a fuzzy member
    fromFuzzy :: [(a, Double)] -> [(a, Double)] -> Double -- Convert a fuzzy member to a double
    

-- Let us define our symbols    
data DCS = NL | NM | ZR | PM | PL deriving (Show)
type FuzzyValue = [(DCS, Double)] -- To make type signatures more clear


-- Next, we say that our new datatype is part of the FuzzyMember class,
-- we implement the weightedFuzzySet and membership functions and we are set!
instance FuzzySymbol DCS where
   
    weightedFuzzySet = [(NL, -1), (NM, -0.5), (ZR, 0), (PM, 0.5), (PL, 1)] -- Symbols and weights
    
    membership x NL 
        | -1 <= x && x <= -0.5 = -2 * x + 1
        | x <= -1 = 1
        | otherwise = 0
    membership x NM
        | -0.5 <= x && x <= 0  = -2 * x
        | -1 <= x && x <= -0.5 =  2 * x + 2
        |otherwise = 0
    membership x ZR 
        | 0 <= x && x <= 0.5  = -2 * x + 1
        | -0.5 <= x && x <= 0 =  2 * x + 1
        | otherwise = 0
    membership x PM 
        | 0 <= x && x <= 0.5 =  2 * x
        | 0.5 <= x && x <= 1 = -2 * x + 2
        | otherwise = 0
    membership x PL
        | 0.5 <= x && x <= 1 = 2 * x - 1
        | 1 <= x = 1
        | otherwise = 0
        
    toFuzzy symbols value = foldl (\a symbol -> a ++ [(symbol, membership value symbol)]) [] symbols
    
    fromFuzzy weightedFuzzySet fuzzyValue = sum weightedFuzzyValue / sum weights
        where weightedFuzzyValue = zipWith (\fv w -> fv * w) weights symbolWeights
              symbolWeights = snd <$> weightedFuzzySet
              weights = snd <$> fuzzyValue


-- Doesn't check if the value provided is normalized or not.
normalize :: FuzzyValue -> FuzzyValue
normalize fuzzyValue = (\(s, v) -> (s, v * norm)) <$> fuzzyValue
    where norm = 1.0 / sum weights
          weights = snd <$> fuzzyValue


--transition :: FLRS -> [FuzzyValue] -> [FuzzyValue]
--transition rulelset inputs = []


-- Defining fuzzyfying and defuzzyfying functions
fuzzyfy = toFuzzy (fuzzySymbols :: [DCS])
defuzzyfy = fromFuzzy (weightedFuzzySet :: [(DCS, Double)])

