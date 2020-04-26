module Arkham.Types.Stack where

import Arkham.Types.Act
import Arkham.Types.Agenda
import GHC.Generics
import Json
import Prelude (Show)

data ArkhamStack = ArkhamStackAgenda ArkhamAgenda | ArkhamStackAct ArkhamAct
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via TaggedJson "stack" ArkhamStack
