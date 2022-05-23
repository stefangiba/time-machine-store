{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter4.Containers where

import qualified Data.Map as M
import qualified Data.Set as S
import           Chapter3.Lists (listOfClients)
import           Chapter2.DataTypes (Client(..))

-- MAPS
insertWithAlter :: Ord k => k -> a -> M.Map k a -> M.Map k a
insertWithAlter k v = M.alter (\item -> Just v) k

deleteWithAlter :: Ord k => k -> M.Map k a -> M.Map k a
deleteWithAlter = M.alter (const Nothing)

adjustWithAlter :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjustWithAlter f = M.alter
  (\case
     Just v -> Just (f v)
     _      -> Nothing)

-- SETS
fromList :: Ord a => [a] -> S.Set a
fromList = S.fromList

insert :: Ord a => a -> S.Set a -> S.Set a
insert = S.insert

toList :: S.Set a -> [a]
toList = S.toList

intersection :: Ord a => S.Set a -> S.Set a -> S.Set a
intersection = S.intersection

data ClientKind = GovOrgKind
                | CompanyKind
                | IndividualKind
  deriving Show

instance Eq ClientKind where
  IndividualKind == IndividualKind = IndividualKind == IndividualKind
  IndividualKind == CompanyKind = IndividualKind /= CompanyKind
  IndividualKind == GovOrgKind = IndividualKind /= GovOrgKind
  CompanyKind == CompanyKind = CompanyKind == CompanyKind
  CompanyKind == IndividualKind = CompanyKind /= IndividualKind
  CompanyKind == GovOrgKind = CompanyKind /= GovOrgKind
  GovOrgKind == GovOrgKind = GovOrgKind == GovOrgKind
  GovOrgKind == IndividualKind = GovOrgKind /= IndividualKind
  GovOrgKind == CompanyKind = GovOrgKind /= CompanyKind

instance Ord ClientKind where
  IndividualKind `compare` _ = GT
  _ `compare` IndividualKind = LT
  CompanyKind `compare` GovOrgKind = LT
  GovOrgKind `compare` CompanyKind = GT
  GovOrgKind `compare` GovOrgKind = EQ
  CompanyKind `compare` CompanyKind = EQ

classifyClients :: [Client] -> M.Map ClientKind (S.Set Client)
classifyClients clients =
  let getClientSet :: ClientKind
                   -> M.Map ClientKind (S.Set Client)
                   -> Maybe (S.Set Client)
      getClientSet clientKind = M.lookup clientKind
      go :: [Client]
         -> M.Map ClientKind (S.Set Client)
         -> M.Map ClientKind (S.Set Client)
      go clients acc = case clients of
        []   -> acc
        x:xs -> case x of
          GovOrg {}     -> case getClientSet GovOrgKind acc of
            Just set -> go xs $ M.insert GovOrgKind (S.insert x set) acc
            Nothing  -> go xs $ M.insert GovOrgKind (S.singleton x) acc
          Company {}    -> case getClientSet CompanyKind acc of
            Just set -> go xs $ M.insert CompanyKind (S.insert x set) acc
            Nothing  -> go xs $ M.insert CompanyKind (S.singleton x) acc
          Individual {} -> case getClientSet IndividualKind acc of
            Just set -> go xs $ M.insert IndividualKind (S.insert x set) acc
            Nothing  -> go xs $ M.insert IndividualKind (S.singleton x) acc
  in go clients M.empty