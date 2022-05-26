{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import           Chapter2.DataTypes (Client(..), Person(..))
import           Chapter4.Containers
import           Data.Map
import           Data.Set
import           GHC.Base
import           Test.QuickCheck

main1 :: IO (Map ClientKind (Set Client))
main1 = do
  clientList <- generate arbitraryClientList
  return (classifyClients1 clientList)

main2 :: IO (Map ClientKind (Set Client))
main2 = do
  clientList <- generate arbitraryClientList
  return (classifyClients2 clientList)

instance Arbitrary Client where
  arbitrary = do
    clientType :: Int <- choose (0, 2)
    id <- arbitrary
    clientName <- arbitrary
    if clientType == 0
      then do
        return (GovOrg id clientName)
      else if clientType == 1
           then do
             duty <- arbitrary
             firstName <- arbitrary
             lastName <- arbitrary
             return (Company id clientName (Person firstName lastName) duty)
           else do
             firstName <- arbitrary
             lastName <- arbitrary
             return (Individual id (Person firstName lastName))

arbitraryClientList :: Gen [Client]
arbitraryClientList = sized
  $ \n -> do
    GHC.Base.sequence [arbitrary | _ <- [1 .. 50000]]
