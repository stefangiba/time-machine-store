module Chapter3.ParametricPolymorphism where

maybeString :: Maybe a -> [Char]
maybeString (Just _) = "Just"
maybeString Nothing = "Nothing"

data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i
                        , clientName :: String
                        , person :: Person
                        , duty :: String
                        }
              | Individual { clientId :: i, person :: Person }
  deriving (Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String }
  deriving (Show, Eq, Ord)

swapTriple :: (a, b, c) -> (b, c, a)
swapTriple (x, y, z) = (y, z, x)

duplicate :: b -> (b, b)
duplicate x = (x, x)

nothing :: p -> Maybe a
nothing _ = Nothing

index :: Num a => [b] -> [(a, b)]
index [] = []
index [x] = [(0, x)]
index (x:xs) = (n + 1, x):indexed
  where
    indexed@((n, _):_) = index xs
