{-# LANGUAGE TemplateHaskell #-}

module Arkham.Action.Additional where

import Arkham.Prelude

import Arkham.Action
import Arkham.Id
import Arkham.Trait
import Data.Aeson.TH
import GHC.OverloadedLabels

data ActionRestriction = AbilitiesOnly | NoRestriction
  deriving stock (Show, Eq, Ord, Data)

data AdditionalAction
  = TraitRestrictedAdditionalAction Trait ActionRestriction
  | ActionRestrictedAdditionalAction Action
  | EffectAction Text EffectId
  | AnyAdditionalAction
  | BountyAction -- Tony Morgan
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "evade" AdditionalAction where
  fromLabel = ActionRestrictedAdditionalAction #evade

additionalActionLabel :: AdditionalAction -> Text
additionalActionLabel = \case
  TraitRestrictedAdditionalAction trait AbilitiesOnly -> "Use on " <> tshow trait <> " abilities"
  TraitRestrictedAdditionalAction trait NoRestriction -> "Use on " <> tshow trait <> " cards"
  ActionRestrictedAdditionalAction action -> "Use on " <> tshow action <> " actions"
  EffectAction label _ -> label
  AnyAdditionalAction -> "Use on any action"
  BountyAction -> "Use to engage or fight an enemy with 1 or more bounties on it."

$(deriveJSON defaultOptions ''ActionRestriction)
$(deriveJSON defaultOptions ''AdditionalAction)
