module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst)
                   then head lst
                   else "empty"

-- (+++) :: [a] -> [a] -> [a]
-- list1 +++ list2 = case list1 of
--   []     -> list2
--   x : xs -> x : (xs +++ list2)
(+++) :: [a] -> [a] -> [a]
[] +++ list2 = list2
(x:xs) +++ list2 = x:(xs +++ list2)

reverse2 :: [a] -> [a]
reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]

-- maxmin :: Ord a => [a] -> (a, a)
-- maxmin list =
--   let h = head list
--   in  if null (tail list)
--         then (h, h)
--         else (if h > t_max then h else t_max, if h < t_min then h else t_min)
--  where
--   t     = maxmin (tail list)
--   t_max = fst t
--   t_min = snd t
maxmin :: Ord a => [a] -> Maybe (a, a)
maxmin [] = Nothing
maxmin [x] = Just (x, x)
maxmin (x:xs) = Just
  ( if x > xs_max
    then x
    else xs_max
  , if x < xs_min
    then x
    else xs_min)
  where
    Just (xs_max, xs_min) = maxmin xs

test :: [a] -> String
test someList = let h = head someList
                in if null (tail someList)
                   then oneElementMessage
                   else moreElementsMessage
  where
    oneElementMessage = "The list has just one element"

    moreElementsMessage = "The list contains multiple elements"

-- fibonacci :: Integer -> Integer
-- fibonacci n = case n of
--   0 -> 0
--   1 -> 1
--   _ -> fibonacci (n - 1) + fibonacci (n - 2)
fibonacci :: (Eq a, Num a, Num p) => a -> p
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- ifibonacci :: Integer -> Maybe Integer
-- ifibonacci n = if n < 0
--   then Nothing
--   else case n of
--     0 -> Just 0
--     1 -> Just 1
--     n' ->
--       let Just f1 = ifibonacci (n' - 1)
--           Just f2 = ifibonacci (n' - 2)
--       in  Just (f1 + f2)
ifibonacci n
  | n < 0 = Nothing
ifibonacci 0 = Just 0
ifibonacci 1 = Just 1
ifibonacci n
  | otherwise = let Just f1 = ifibonacci (n - 1)
                    Just f2 = ifibonacci (n - 2)
                in Just (f1 + f2)

sorted :: [Integer] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:r@(y:_)) = x < y && sorted r

binom :: (Eq a, Num a, Num p) => a -> a -> p
binom _ 0 = 1
binom x y
  | x == y = 1
binom n k = (binom (n - 1) (k - 1)) + (binom (n - 1) k)

ackermann :: (Num a, Num t, Ord a, Ord t) => a -> t -> t
ackermann m n
  | m == 0 = n + 1
ackermann m n
  | m > 0 && n == 0 = ackermann (m - 1) 1
ackermann m n
  | m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))

unzip :: [(a, a)] -> ([a], [a])
unzip list = case list of
  x0:x1:xs -> ([fst x0, snd x0], [fst x1, snd x1])
  _        -> ([], [])

myUnzip :: [(a1, a2)] -> ([a1], [a2])
myUnzip [] = ([], [])
myUnzip ((a, b):xs) = (a:x1, b:x2)
  where
    (x1, x2) = myUnzip xs
