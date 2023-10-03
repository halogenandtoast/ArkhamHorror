{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Action.Additional where

import Arkham.Prelude

import Arkham.Action
import Arkham.Id
import {-# SOURCE #-} Arkham.Source
import Arkham.Trait
import Data.Aeson.TH
import GHC.OverloadedLabels

data ActionRestriction = AbilitiesOnly | NoRestriction
  deriving stock (Show, Eq, Ord, Data)

data AdditionalActionType
  = TraitRestrictedAdditionalAction Trait ActionRestriction
  | ActionRestrictedAdditionalAction Action
  | EffectAction Text EffectId
  | AnyAdditionalAction
  | BountyAction -- Tony Morgan
  deriving stock (Show, Eq, Ord, Data)

data AdditionalAction = AdditionalAction {label :: Text, source :: Source, kind :: AdditionalActionType}
  deriving stock (Show, Eq, Ord, Data)

additionalActionType :: AdditionalAction -> AdditionalActionType
additionalActionType (AdditionalAction _ _ aType) = aType

additionalActionSource :: AdditionalAction -> Source
additionalActionSource (AdditionalAction _ aSource _) = aSource

additionalActionLabel :: AdditionalAction -> Text
additionalActionLabel (AdditionalAction aLabel _ _) = aLabel

instance IsLabel "evade" AdditionalActionType where
  fromLabel = ActionRestrictedAdditionalAction #evade

instance IsLabel "fight" AdditionalActionType where
  fromLabel = ActionRestrictedAdditionalAction #fight

instance IsLabel "explore" AdditionalActionType where
  fromLabel = ActionRestrictedAdditionalAction #explore

instance IsLabel "any" AdditionalActionType where
  fromLabel = AnyAdditionalAction

-- additionalActionLabel :: AdditionalAction -> Text
-- additionalActionLabel (AdditionalAction _ aType) = case aType of
--   TraitRestrictedAdditionalAction trait AbilitiesOnly -> "Use on " <> tshow trait <> " abilities"
--   TraitRestrictedAdditionalAction trait NoRestriction -> "Use on " <> tshow trait <> " cards"
--   ActionRestrictedAdditionalAction action -> "Use on " <> tshow action <> " actions"
--   EffectAction label _ -> label
--   AnyAdditionalAction -> "Use on any action"
--   BountyAction -> "Use to engage or fight an enemy with 1 or more bounties on it."

$(deriveJSON defaultOptions ''ActionRestriction)
$(deriveJSON defaultOptions ''AdditionalActionType)
$(deriveJSON defaultOptions ''AdditionalAction)
