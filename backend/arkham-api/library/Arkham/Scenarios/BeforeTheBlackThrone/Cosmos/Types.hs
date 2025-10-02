module Arkham.Scenarios.BeforeTheBlackThrone.Cosmos.Types where

import Arkham.Prelude

data Pos = Pos Int Int
  deriving stock (Ord, Eq, Show, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

-- Todo hardcode a and b, store location id for empty space
data CosmosLocation a b = EmptySpace Pos a | CosmosLocation Pos b
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data CosmosRow a b
  = CosmosRow
      (Seq (Maybe (CosmosLocation a b)))
      (Maybe (CosmosLocation a b))
      (Seq (Maybe (CosmosLocation a b)))
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data Cosmos a b = Cosmos
  { cosmosAbove :: Seq (CosmosRow a b)
  , cosmosCenter :: CosmosRow a b
  , cosmosBelow :: Seq (CosmosRow a b)
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
