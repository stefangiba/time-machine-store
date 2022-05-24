{-# LANGUAGE ViewPatterns #-}

module Chapter2.DataTypes where

import           Control.Arrow (Arrow(first))

data Client = GovOrg { clientId :: Integer, clientName :: String }
            | Company { clientId :: Integer
                      , clientName :: String
                      , person :: Person
                      , duty :: String
                      }
            | Individual { clientId :: Integer, person :: Person }
  deriving (Show)

instance Eq Client where
  Individual clientId1 person1 == Individual clientId2 person2 =
    (clientId1 == clientId2)
    && (firstName person1 == firstName person2)
    && (lastName person1 == lastName person2)
  Individual {} == GovOrg {} = False
  Individual {} == Company {} = False
  GovOrg clientId1 clientName1 == GovOrg clientId2 clientName2 =
    (clientId1 == clientId2) && (clientName1 == clientName2)
  GovOrg {} == Individual {} = False
  GovOrg {} == Company {} = False
  Company clientId1 clientName1 person1 duty1
    == Company clientId2 clientName2 person2 duty2 = (clientId1 == clientId2)
    && (firstName person1 == firstName person2)
    && (lastName person1 == lastName person2)
    && (clientName1 == clientName2)
    && (duty1 == duty2)
  Company {} == Individual {} = False
  Company {} == GovOrg {} = False

instance Ord Client where
  Individual { person = p1 } `compare` Individual { person = p2 } =
    compare (firstName p1 ++ lastName p1) (firstName p2 ++ lastName p2)
  Individual {} `compare` _ = GT
  _ `compare` Individual {} = LT
  Company { clientName = c1 }
    `compare` Company { clientName = c2 } = compare c1 c2
  GovOrg { clientName = c1 }
    `compare` GovOrg { clientName = c2 } = compare c1 c2
  Company { clientName = c1 }
    `compare` GovOrg { clientName = c2 } = compare c1 c2
  GovOrg { clientName = c1 }
    `compare` Company { clientName = c2 } = compare c1 c2

data Person = Person { firstName :: String, lastName :: String }
  deriving Show

data Gender = Male
            | Female
            | Unknown
  deriving (Show)

data TimeMachine = TimeMachine { manufacturer :: String
                               , model :: Model
                               , specs :: Specs
                               , price :: Float
                               }
  deriving Show

data Model = Model { modelId :: Integer, modelName :: String }
  deriving Show

data Specs = Specs { travelsToFuture :: Bool, travelsToPast :: Bool }
  deriving Show

name :: Client -> String
name client = case client of
  GovOrg _ name       -> clientName client
  Company _ name _ _  -> clientName client
  Individual _ person -> firstName person ++ " " ++ lastName person
