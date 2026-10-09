{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Action.Additional where

import Arkham.Action
import Arkham.Id
import Arkham.Prelude

-- import {-# SOURCE #-} Arkham.Matcher.Types
import Arkham.Matcher.Types
import {-# SOURCE #-} Arkham.Source
import Arkham.Trait
import Data.Aeson.TH
import GHC.OverloadedLabels

data ActionRestriction = AbilitiesOnly | NoRestriction
  deriving stock (Show, Eq, Ord, Data)

data AdditionalActionType
  = TraitRestrictedAdditionalAction Trait ActionRestriction
  | ActionRestrictedAdditionalAction Action
  | AbilityRestrictedAdditionalAction Source Int
  | PlayCardRestrictedAdditionalAction ExtendedCardMatcher
  | {- | Spendable only on an ability the matcher accepts, whoever it belongs to.
    'AbilityRestrictedAdditionalAction' names one ability of one card; this
    names a shape of ability, which is what an investigator who may take an
    extra action "to activate an ability on an asset you control" is granted.
    -}
    AbilityMatchingAdditionalAction AbilityMatcher
  | EffectAction Text EffectId
  | AnyAdditionalAction
  | BountyAction -- Tony Morgan
  | BobJenkinsAction -- Bob Jenkins... probably
  deriving stock (Show, Eq, Ord, Data)

data AdditionalAction = AdditionalAction {label :: Text, source :: Source, kind :: AdditionalActionType}
  deriving stock (Show, Eq, Ord, Data)

additionalActionType :: AdditionalAction -> AdditionalActionType
additionalActionType (AdditionalAction _ _ aType) = aType

additionalActionSource :: AdditionalAction -> Source
additionalActionSource (AdditionalAction _ aSource _) = aSource

{- | Is this an additional /standard/ action? Per the Ages Unwound campaign guide:

> A standard action is any action that does not have a limitation on its use,
> regardless of its source. For example, Finn Edwards has a copy of Leo De Luca
> in play. He has four standard actions on his turns - the default three and an
> additional action from Leo - as well as an additional, non-standard action
> from his investigator ability that can only be used to evade.

So an investigator's standard actions are their remaining actions plus every
'AdditionalAction' whose kind is 'AnyAdditionalAction'; every other
'AdditionalActionType' carries a limitation on its use.
-}
isStandardAdditionalAction :: AdditionalAction -> Bool
isStandardAdditionalAction a = additionalActionType a == AnyAdditionalAction

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
