module Arkham.Types.Act where

import Arkham.Types.Card
import GHC.Generics
import Json
import Prelude (Show)

newtype ArkhamAct = ArkhamAct { arkhamActCurrentCard :: ArkhamActCard }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamAct") ArkhamAct

