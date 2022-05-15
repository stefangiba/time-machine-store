{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- {-# LANGUAGE ViewPatterns #-}
module Chapter3.FunctionsAsParameters where

import           Chapter3.Patterns (Range(..), pattern Range, range)
-- import           Chapter3.Patterns (Range, RangeObs(..), r, range)
import           Chapter2.DataTypes (Client(..))

apply3f2 :: (Num a, Num t) => (t -> a) -> t -> a
apply3f2 f x = 3 * f (x + 2)

equalTuples :: Eq a => [(a, a)] -> [Bool]
equalTuples = map (Prelude.uncurry (==))

sayHello :: [String] -> [String]
sayHello = map
  (\case
     "Alejandro" -> "Hello, writer"
     name        -> "Welcome, " ++ name)

multiplyByN :: Num a => a -> (a -> a)
multiplyByN n = (n *)

filterOnes :: (Eq a, Num a) => [a] -> [a]
filterOnes = filter (== 1)

filterANumber :: Eq a => a -> [a] -> [a]
filterANumber number = filter (== number)

filterNot :: (t -> Bool) -> [t] -> [t]
filterNot predicate = filter (not . predicate)

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter
  (\case
     GovOrg _ _ -> True
     _          -> False)

duplicateOdds :: [Integer] -> [Integer]
duplicateOdds = map (* 2) . filter odd

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

rangesFromTuples :: [(Integer, Integer)] -> [Range]
rangesFromTuples = map (Chapter3.FunctionsAsParameters.uncurry range)

-- PatternSynonyms
match :: Range -> (Integer, Integer)
match rng = case rng of
  Range a b -> (a, b)

-- ViewPatterns
-- match :: Range -> (Integer, Integer)
-- match rng = case rng of
--   (r -> R a b) -> (a, b)
--PatternSynonyms
prettyRange :: Range -> String
prettyRange rng = case rng of
  Range a b -> "[" ++ show a ++ ", " ++ show b ++ "]"
-- ViewPatterns
-- prettyRange :: Range -> String
-- prettyRange rng = case rng of
--   (r -> R a b) -> "[" ++ show a ++ ", " ++ show b ++ "]"
