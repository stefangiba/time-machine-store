{-# LANGUAGE ViewPatterns #-}

module Chapter2.DataTypes where

data Client = GovOrg { clientId :: Integer, clientName :: String }
            | Company { clientId :: Integer
                      , clientName :: String
                      , person :: Person
                      , duty :: String
                      }
            | Individual { clientId :: Integer, person :: Person }
  deriving (Show)

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
