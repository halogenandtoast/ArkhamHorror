{-# LANGUAGE TemplateHaskell #-}
module Arkham.Action.Additional where

import Arkham.Prelude

import Arkham.Action
import Arkham.Id
import Arkham.Trait
import Data.Aeson.TH

data ActionRestriction = AbilitiesOnly | NoRestriction
  deriving stock (Show, Eq, Ord)

data AdditionalAction
  = TraitRestrictedAdditionalAction Trait ActionRestriction
  | ActionRestrictedAdditionalAction Action
  | EffectAction Text EffectId
  | AnyAdditionalAction
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions ''ActionRestriction)
$(deriveJSON defaultOptions ''AdditionalAction)
