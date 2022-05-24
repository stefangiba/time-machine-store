{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TransformListComp #-}

module Chapter3.Lists where

import           Chapter2.DataTypes (Client(..)
                                   , Person(Person, firstName, lastName))
import           Data.Bits (Bits(xor))
import           Data.Char (toUpper)
import           Data.Function (on)
import           Data.List (groupBy, sortBy)
import           GHC.Exts (groupWith, sortWith, the)

filter :: (a -> Bool) -> [a] -> [a]
filter predicate list =
  let go initialList filtered = case initialList of
        []   -> filtered
        x:xs -> if predicate x
                then go xs (x:filtered)
                else go xs filtered
  in go list []

-- LIST FOLDS
data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity

maxFromList :: (Num a, Ord a) => [a] -> Maybe a
maxFromList list =
  let infMax :: (Num a, Ord a) => InfNumber a -> InfNumber a -> InfNumber a
      infMax =
        let aux :: (Num a, Ord a) => (InfNumber a, InfNumber a) -> InfNumber a
            aux tup = case tup of
              (MinusInfinity, x)   -> x
              (x, MinusInfinity)   -> x
              (PlusInfinity, _)    -> PlusInfinity
              (_, PlusInfinity)    -> PlusInfinity
              (Number a, Number b) -> Number (max a b)
        in curry aux
  in case foldr (infMax . Number) MinusInfinity list of
       Number a      -> Just a
       MinusInfinity -> Nothing
       PlusInfinity  -> Nothing

-- product :: Num t => [t] -> t
-- product list = let
--     aux list acc = case list of
--       [] -> acc
--       x:xs -> aux xs (x * acc)
--   in
--     aux list 1
product :: (Num a) => [a] -> a
product = foldl (*) 1

-- minimumClient :: [Client] -> Maybe Client
-- minimumClient clients =
--   let aux clientList (minNameLength, client) = case clientList of
--         []   -> client
--         x:xs -> let clientNameLength = length (clientName x)
--                     min = ( if clientNameLength < minNameLength
--                             then clientNameLength
--                             else minNameLength
--                           , x)
--                 in aux clientList min
--   in case clients of
--        []   -> Nothing
--        x:xs -> Just (aux xs (length (clientName x), x))
--   where
--     x = maxBound :: Int
minimumClient :: [Client] -> Maybe Client
minimumClient clients = case clients of
  [] -> Nothing
  _  -> Just
    (foldl1
       (\x y -> let l1 = length (clientName x)
                    l2 = length (clientName y)
                in if l1 < l2
                   then x
                   else y)
       clients)

-- all :: [Bool] -> Bool
-- all = let aux acc list = case list of
--             []   -> acc
--             x:xs -> aux (x && acc) xs
--       in aux True
all :: [Bool] -> Maybe Bool
all booleanList = case booleanList of
  [] -> Nothing
  _  -> Just (and booleanList)

minimumBy :: (Ord a, Ord b) => (a -> b) -> [a] -> a
minimumBy f = foldl1
  (\x y -> if f x < f y
           then x
           else y)

-- FILTERS WITH BOOLEAN PREDICATES
bothFilters :: (a -> Bool) -> [a] -> ([a], [a])
bothFilters p list = (Prelude.filter p list, Prelude.filter (not . p) list)

skipUntilGov :: [Client] -> [Client]
skipUntilGov = dropWhile
  (\case
     GovOrg {} -> False
     _         -> True)

elem :: Eq t => t -> [t] -> Bool
elem element list = case list of
  []   -> False
  x:xs -> (x == element) || Chapter3.Lists.elem element xs

compareClient :: Client -> Client -> Ordering
compareClient Individual { person = p1 } Individual { person = p2 } =
  compare (firstName p1 ++ lastName p1) (firstName p2 ++ lastName p2)
compareClient Individual {} _ = GT
compareClient _ Individual {} = LT
compareClient c1 c2 = compare (clientName c1) (clientName c2)

listOfClients :: [Client]
listOfClients =
  [ Individual 1 (Person "H.G." "Wells")
  , GovOrg 2 "NTTF"
  , Company 3 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist"
  , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist"
  , Company 5 "Boring Company" (Person "Elon" "Musk") "CEO"
  , Individual 6 (Person "Doctor" "")
  , Individual 7 (Person "Sarah" "Jane")]

companyDutiesAnalytics :: [Client] -> [String]
companyDutiesAnalytics = map (duty . head)
  . sortBy (flip compare `on` length)
  . groupBy ((==) `on` duty)
  . Prelude.filter isCompany
  where
    isCompany Company {} = True
    isCompany _ = False

-- LISTS CONTAINING TUPLES
enum :: Int -> Int -> [Int]
enum a b
  | a > b = []
enum a b = a:enum (a + 1) b

withPositions :: [a] -> [(Int, a)]
-- withPositions list = zip (enum 1 $ length list) list
withPositions list = zip [1 .. length list] list

-- LIST COMPREHENSIONS
duplicateOdds :: Integral a => [a] -> [a]
duplicateOdds list = [2 * x | x <- list, odd x]

getNameOfGovOrgs :: [Client] -> [String]
getNameOfGovOrgs clients = [clientName x | x@(GovOrg _ _) <- clients]

getProductsList :: [(Integer, Integer, Integer)]
getProductsList = [(x, y, x * y) | x <- [1 .. 4], y <- [1 .. 10]]

dependentGenerator :: [(Integer, Integer)]
dependentGenerator = [(x, y) | x <- [0 .. 6], y <- [x .. 6]]

stringListToUpper :: [[Char]] -> [Char]
stringListToUpper list = [toUpper c | s <- list, c <- ' ':s]

localBindingListComp :: [Double]
localBindingListComp =
  [sqrt v | (x, y) <- [(1, 2), (3, 8)], let v = x * x + y * y]

listCompWithGuard :: [(Integer, Integer)]
listCompWithGuard = [(x, y) | x <- [1 .. 6], y <- [1 .. 6], x < y]

transformListComp :: [Integer]
transformListComp = [x * y | x <- [-1, 1, -2], y <- [1, 2, 3], then reverse]

sortWithListComp :: [Integer]
sortWithListComp =
  [x * y | x <- [-1, 1, -2], y <- [1, 2, 3], then sortWith by x]

groupByListComp :: [(Bool, [Integer])]
groupByListComp = [(the p, m)
                  | x <- [-1, 1, -2]
                  , y <- [1, 2, 3]
                  , let m = x * y
                  , let p = m > 0
                  , then group by p using groupWith]

companyAnalytics :: [Client] -> [(String, [(Person, String)])]
companyAnalytics clients =
  [(the clientName, zip person duty)
  | client@Company { .. } <- clients
  , then sortWith by duty
  , then group by clientName using groupWith
  , then sortWith by length client]

filterAsFold :: Foldable t => (a -> Bool) -> t a -> [a]
filterAsFold predicate = foldr
  (\x l -> if predicate x
           then x:l
           else l)
  []

mapAsFold :: Foldable t => (a -> b) -> t a -> [b]
mapAsFold func = foldr (\x l -> func x:l) []
