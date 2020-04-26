module Arkham.Types.Action where

import GHC.Generics
import Json
import Prelude (Int, Show)

data ArkhamAction = ArkhamActionRevealLocation Int | ArkhamActionInvestigate Int
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "action" ArkhamAction

