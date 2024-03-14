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
import GHC.Records

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
  , abilityDelayAdditionalCosts :: Bool
  }
  deriving stock (Show, Ord, Data)

instance HasField "source" Ability Source where
  getField = abilitySource

instance HasField "index" Ability Int where
  getField = abilityIndex

data AbilityMetadata
  = IntMetadata Int
  | TargetMetadata Target
  | SourceMetadata Source
  | EncounterCardMetadata EncounterCard
  | SkillChoiceMetadata SkillType
  | NoAbilityMetadata
  | InvestigateTargets LocationMatcher
  deriving stock (Eq, Show, Ord, Data)

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

abilityDelayAdditionalCostsL :: Lens' Ability Bool
abilityDelayAdditionalCostsL = lens abilityDelayAdditionalCosts $ \m x -> m {abilityDelayAdditionalCosts = x}

$(deriveJSON defaultOptions ''AbilityMetadata)
$(deriveToJSON (aesonOptions $ Just "ability") ''Ability)

instance FromJSON Ability where
  parseJSON = withObject "Ability" $ \o -> do
    abilitySource <- o .: "source"
    abilityIndex <- o .: "index"
    abilityType <- o .: "type"
    abilityLimit <- o .: "limit"
    abilityWindow <- o .: "window"
    abilityMetadata <- o .:? "metadata"
    abilityCriteria <- o .: "criteria"
    abilityDoesNotProvokeAttacksOfOpportunity <- o .: "doesNotProvokeAttacksOfOpportunity"
    abilityTooltip <- o .:? "tooltip"
    abilityCanBeCancelled <- o .: "canBeCancelled"
    abilityDisplayAsAction <- o .: "displayAsAction"
    abilityDelayAdditionalCosts <- o .:? "delayAdditionalCosts" .!= False
    pure Ability {..}
