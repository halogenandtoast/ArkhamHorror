module Arkham.Types.ChaosToken where

import Arkham.Types.Simple
import GHC.Generics
import Json
import Prelude (Show)

data ArkhamChaosTokenDifficulties = ArkhamChaosTokenDifficulties
  { arkhamChaosTokenDifficultiesEasy :: [ArkhamChaosToken]
  , arkhamChaosTokenDifficultiesStandard :: [ArkhamChaosToken]
  , arkhamChaosTokenDifficultiesHard :: [ArkhamChaosToken]
  , arkhamChaosTokenDifficultiesExpert :: [ArkhamChaosToken]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamChaosTokenDifficulties") ArkhamChaosTokenDifficulties
