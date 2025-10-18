{-# LANGUAGE TemplateHaskell #-}

module Arkham.Ability.Types where

import Arkham.Prelude

import Arkham.Ability.Limit
import Arkham.Ability.Type
import Arkham.Action
import Arkham.Card.CardCode
import Arkham.Card.EncounterCard
import Arkham.Cost
import Arkham.Criteria (Criterion (NoRestriction))
import Arkham.Json
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data AdditionalCostDelay = DelayAdditionalCosts | DelayAdditionalCostsWhen Criterion
  deriving stock (Show, Eq, Ord, Data)

data AbilityDisplayAs = DisplayAsAction | DisplayAsCard
  deriving stock (Show, Eq, Ord, Data)

data Ability = Ability
  { abilitySource :: Source
  , abilityCardCode :: CardCode
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityLimit :: AbilityLimit
  , abilityWindow :: WindowMatcher
  , abilityMetadata :: Maybe AbilityMetadata
  , abilityCriteria :: Criterion
  , abilityDoesNotProvokeAttacksOfOpportunity :: Bool
  , abilityTooltip :: Maybe Text
  , abilityCanBeCancelled :: Bool
  , abilityDisplayAs :: Maybe AbilityDisplayAs
  , abilityDelayAdditionalCosts :: Maybe AdditionalCostDelay
  , abilityBasic :: Bool
  , abilityAdditionalCosts :: [Cost]
  , abilityRequestor :: Source
  , abilityTriggersSkillTest :: Bool
  , abilityWantsSkillTest :: Maybe SkillTestMatcher
  , abilityTarget :: Maybe Target -- used to highlight the target of the ability in the UI
  }
  deriving stock (Show, Ord, Data)

overAbilityActions :: ([Action] -> [Action]) -> Ability -> Ability
overAbilityActions f ab = ab {Arkham.Ability.Types.abilityType = overAbilityTypeActions f ab.kind}

buildFightAbility :: (Sourceable source, HasCardCode source) => source -> Int -> Ability
buildFightAbility source idx =
  (buildAbility source idx (ActionAbility [#fight] Free))
    { abilityDoesNotProvokeAttacksOfOpportunity = True
    }

buildAbility :: (Sourceable source, HasCardCode source) => source -> Int -> AbilityType -> Ability
buildAbility source idx abilityType =
  Ability
    { abilitySource = toSource source
    , abilityCardCode = toCardCode source
    , abilityIndex = idx
    , abilityType = abilityType
    , abilityLimit = NoLimit
    , abilityWindow = AnyWindow
    , abilityMetadata = Nothing
    , abilityCriteria = NoRestriction
    , abilityDoesNotProvokeAttacksOfOpportunity = False
    , abilityTooltip = Nothing
    , abilityCanBeCancelled = True
    , abilityDisplayAs = Nothing
    , abilityDelayAdditionalCosts = Nothing
    , abilityBasic = False
    , abilityAdditionalCosts = []
    , abilityRequestor = toSource source
    , abilityTriggersSkillTest = False
    , abilityWantsSkillTest = Nothing
    , abilityTarget = Nothing
    }

withHighlight :: Targetable target => target -> Ability -> Ability
withHighlight target ab = ab {abilityTarget = Just (toTarget target)}

skillTestAbility :: Ability -> Ability
skillTestAbility ab = ab {abilityTriggersSkillTest = True}

notSkillTestAbility :: Ability -> Ability
notSkillTestAbility ab = ab {abilityTriggersSkillTest = False}

setRequestor :: Sourceable source => source -> Ability -> Ability
setRequestor source ab = ab {abilityRequestor = toSource source}

instance HasCost Ability where
  overCost f ab = ab {Arkham.Ability.Types.abilityType = overCost f (abilityType ab)}

instance HasField "wantsSkillTest" Ability (Maybe SkillTestMatcher) where
  getField = abilityWantsSkillTest

instance HasField "limitType" Ability (Maybe AbilityLimitType) where
  getField = abilityLimitType . abilityLimit

instance HasField "criteria" Ability Criterion where
  getField = abilityCriteria

instance HasField "kind" Ability AbilityType where
  getField = abilityType

instance HasField "requestor" Ability Source where
  getField = abilityRequestor

instance HasField "source" Ability Source where
  getField = abilitySource

instance HasField "cardCode" Ability CardCode where
  getField = abilityCardCode

instance HasField "index" Ability Int where
  getField = abilityIndex

instance HasField "ref" Ability AbilityRef where
  getField = abilityToRef

data AbilityRef = AbilityRef Source Int
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

abilityToRef :: Ability -> AbilityRef
abilityToRef a = AbilityRef a.source a.index

instance HasField "source" AbilityRef Source where
  getField (AbilityRef s _) = s

instance HasField "index" AbilityRef Int where
  getField (AbilityRef _ idx) = idx

data AbilityMetadata
  = IntMetadata Int
  | TargetMetadata Target
  | SourceMetadata Source
  | EncounterCardMetadata EncounterCard
  | SkillChoiceMetadata SkillType
  | NoAbilityMetadata
  | InvestigateTargets LocationMatcher
  | CardCodeMetadata CardCode
  deriving stock (Eq, Show, Ord, Data)

instance Eq Ability where
  a == b = (abilitySource a == abilitySource b) && (abilityIndex a == abilityIndex b)

instance Sourceable Ability where
  toSource a = AbilitySource (abilitySource a) (abilityIndex a)

abilityLimitL :: Lens' Ability AbilityLimit
abilityLimitL = lens abilityLimit $ \m x -> m {abilityLimit = x}

abilityTypeL :: Lens' Ability AbilityType
abilityTypeL = lens abilityType $ \m x -> m {Arkham.Ability.Types.abilityType = x}

abilityMetadataL :: Lens' Ability (Maybe AbilityMetadata)
abilityMetadataL = lens abilityMetadata $ \m x -> m {abilityMetadata = x}

abilityTooltipL :: Lens' Ability (Maybe Text)
abilityTooltipL = lens abilityTooltip $ \m x -> m {abilityTooltip = x}

abilityCriteriaL :: Lens' Ability Criterion
abilityCriteriaL = lens abilityCriteria $ \m x -> m {abilityCriteria = x}

abilityWantsSkillTestL :: Lens' Ability (Maybe SkillTestMatcher)
abilityWantsSkillTestL = lens abilityWantsSkillTest $ \m x -> m {abilityWantsSkillTest = x}

abilityDoesNotProvokeAttacksOfOpportunityL :: Lens' Ability Bool
abilityDoesNotProvokeAttacksOfOpportunityL =
  lens abilityDoesNotProvokeAttacksOfOpportunity
    $ \m x -> m {abilityDoesNotProvokeAttacksOfOpportunity = x}

abilityDisplayAsL :: Lens' Ability (Maybe AbilityDisplayAs)
abilityDisplayAsL = lens abilityDisplayAs $ \m x -> m {abilityDisplayAs = x}

abilityDelayAdditionalCostsL :: Lens' Ability (Maybe AdditionalCostDelay)
abilityDelayAdditionalCostsL = lens abilityDelayAdditionalCosts $ \m x -> m {abilityDelayAdditionalCosts = x}

delayAdditionalCosts :: Ability -> Ability
delayAdditionalCosts = abilityDelayAdditionalCostsL ?~ DelayAdditionalCosts

delayAdditionalCostsWhen :: Criterion -> Ability -> Ability
delayAdditionalCostsWhen c = abilityDelayAdditionalCostsL ?~ DelayAdditionalCostsWhen c

mconcat
  [ deriveJSON defaultOptions ''AdditionalCostDelay
  , deriveJSON defaultOptions ''AbilityMetadata
  , deriveJSON defaultOptions ''AbilityDisplayAs
  , deriveToJSON (aesonOptions $ Just "ability") ''Ability
  ]

instance FromJSON Ability where
  parseJSON = withObject "Ability" $ \o -> do
    abilitySource <- o .: "source"
    abilityCardCode <- o .: "cardCode"
    abilityIndex <- o .: "index"
    abilityType <- o .: "type"
    abilityLimit <- o .: "limit"
    abilityWindow <- o .: "window"
    abilityMetadata <- o .:? "metadata"
    abilityCriteria <- o .: "criteria"
    abilityDoesNotProvokeAttacksOfOpportunity <- o .: "doesNotProvokeAttacksOfOpportunity"
    abilityTooltip <- o .:? "tooltip"
    abilityCanBeCancelled <- o .: "canBeCancelled"
    abilityDisplayAsAction <- o .:? "displayAsAction" .!= False
    abilityDisplayAs <-
      o .:? "displayAs" .!= (guard abilityDisplayAsAction $> DisplayAsAction)
    abilityDelayAdditionalCosts <-
      ((o .:? "delayAdditionalCosts" .!= False) <&> \x -> guard x $> DelayAdditionalCosts)
        <|> o
        .:? "delayAdditionalCosts"
    abilityBasic <- o .: "basic"
    abilityAdditionalCosts <- o .: "additionalCosts"
    abilityRequestor <- o .:? "requestor" .!= abilitySource
    abilityTriggersSkillTest <- o .:? "triggersSkillTest" .!= False
    abilityWantsSkillTest <- o .:? "wantsSkillTest" .!= Nothing
    abilityTarget <- o .:? "target"

    pure Ability {..}

newtype DifferentAbility = DifferentAbility AbilityRef
  deriving newtype (Show, ToJSON, FromJSON)

instance HasField "different" Ability DifferentAbility where
  getField ab = DifferentAbility (abilityToRef ab)

instance HasField "different" AbilityRef DifferentAbility where
  getField = DifferentAbility

instance Eq DifferentAbility where
  (DifferentAbility a) == (DifferentAbility b) =
    case a.index of
      100 -> b.index == 100 && sameSource
      101 -> b.index == 101 && sameSource
      102 -> b.index == 102 && sameSource
      _ -> (a.source == b.source) && (a.index == b.index)
   where
    sameSource = case a.source of
      EnemySource _ -> case b.source of
        EnemySource _ -> True
        _ -> False
      LocationSource _ -> case b.source of
        LocationSource _ -> True
        _ -> False
      InvestigatorSource _ -> case b.source of
        InvestigatorSource _ -> True
        _ -> False
      _ -> error $ "Unhandled samesource in DifferentAbility: " <> show (a.source, b.source)
