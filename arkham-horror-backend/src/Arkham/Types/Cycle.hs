module Arkham.Types.Cycle where

import Arkham.Types.ChaosToken
import Data.Text
import GHC.Generics
import Json
import Prelude (Show)

data ArkhamCycle = ArkhamCycle
  { cycleId :: Text
  , cycleName :: Text
  , cycleChaosTokens :: ArkhamChaosTokenDifficulties
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "cycle") ArkhamCycle
