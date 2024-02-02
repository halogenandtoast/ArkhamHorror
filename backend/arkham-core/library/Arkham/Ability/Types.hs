{-# LANGUAGE TemplateHaskell #-}

module Arkham.Ability.Types where

import Arkham.Prelude

import Arkham.Ability.Limit
import Arkham.Ability.Type hiding (abilityType)
import Arkham.Card.EncounterCard
import Arkham.Criteria (Criterion)
import Arkham.Json
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

data Ability = Ability
  { abilitySource :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityLimit :: AbilityLimit
  , abilityWindow :: WindowMatcher
  , abilityMetadata :: Maybe AbilityMetadata
  , abilityCriteria :: Criterion
  , abilityDoesNotProvokeAttacksOfOpportunity :: Bool
  , abilityTooltip :: Maybe Text
  , abilityCanBeCancelled :: Bool
  , abilityDisplayAsAction :: Bool
  }
  deriving stock (Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

data AbilityMetadata
  = IntMetadata Int
  | TargetMetadata Target
  | SourceMetadata Source
  | EncounterCardMetadata EncounterCard
  | SkillChoiceMetadata SkillType
  | NoAbilityMetadata
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

instance Eq Ability where
  a == b =
    (abilitySource a == abilitySource b) && (abilityIndex a == abilityIndex b)

instance Sourceable Ability where
  toSource a = AbilitySource (abilitySource a) (abilityIndex a)

abilityLimitL :: Lens' Ability AbilityLimit
abilityLimitL = lens abilityLimit $ \m x -> m {abilityLimit = x}

abilityTypeL :: Lens' Ability AbilityType
abilityTypeL = lens abilityType $ \m x -> m {abilityType = x}

abilityMetadataL :: Lens' Ability (Maybe AbilityMetadata)
abilityMetadataL = lens abilityMetadata $ \m x -> m {abilityMetadata = x}

abilityTooltipL :: Lens' Ability (Maybe Text)
abilityTooltipL = lens abilityTooltip $ \m x -> m {abilityTooltip = x}

abilityCriteriaL :: Lens' Ability Criterion
abilityCriteriaL = lens abilityCriteria $ \m x -> m {abilityCriteria = x}

abilityDoesNotProvokeAttacksOfOpportunityL :: Lens' Ability Bool
abilityDoesNotProvokeAttacksOfOpportunityL =
  lens abilityDoesNotProvokeAttacksOfOpportunity
    $ \m x -> m {abilityDoesNotProvokeAttacksOfOpportunity = x}

abilityDisplayAsActionL :: Lens' Ability Bool
abilityDisplayAsActionL = lens abilityDisplayAsAction $ \m x -> m {abilityDisplayAsAction = x}

$(deriveJSON defaultOptions ''AbilityMetadata)
$(deriveJSON (aesonOptions $ Just "ability") ''Ability)
