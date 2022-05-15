{-# LANGUAGE PatternSynonyms #-}

module Chapter3.Patterns (Range, pattern Range, range) where

-- module Chapter3.Patterns (Range, RangeObs(..), r, range) where
data Range = MkRange Integer Integer

instance Show Range where
  show (MkRange a b) = "Range " ++ show a ++ " " ++ show b

-- smart constructor
-- PatternSynonym compatible
range :: Integer -> Integer -> Range
range a b = if a <= b
            then MkRange a b
            else error "a must be <= b"

-- ViewPatterns compatible
-- range :: Integer -> Integer -> Range
-- range a b = if a <= b
--             then MkRange a b
--             else error "a must be <= b"
-- ViewPattern
-- data RangeObs = R Integer Integer
--   deriving Show
-- r :: Range -> RangeObs
-- r (MkRange a b) = R a b
-- PatternSynonym
pattern Range :: Integer -> Integer -> Range
pattern Range a b <- MkRange a b
  where
    Range a b = range a b

{-# COMPLETE Range #-}
