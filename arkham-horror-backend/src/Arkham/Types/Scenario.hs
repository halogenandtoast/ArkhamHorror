module Arkham.Types.Scenario where

import Arkham.Types.Simple
import Data.Map.Strict (Map)
import Data.Text
import Database.Persist.Sql
import GHC.Generics
import Json
import Prelude (Int, Show, (<$>), ($), (<>))

newtype ArkhamCardTrait = ArkhamCardTrait { unArkhamCardTrait :: Text }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ArkhamCardType = ArkhamCardType { unArkhamCardType :: Text }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ArkhamLocationMatcher = ArkhamLocationMatcherCurrentLocation
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via TaggedJson "locationMatch" ArkhamLocationMatcher

data ArkhamPenaltyType = ArkhamPenaltyTypeHorror | ArkhamPenaltyTypeDamage
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via TaggedJson "penaltyType" ArkhamPenaltyType

data ArkhamPenalty = ArkhamPenalty
  { arkhamPenaltyType :: ArkhamPenaltyType
  , arkhamPenaltyAmount :: Int
  , arkhamPenaltyCriteria :: ArkhamMatch
  }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via Codec (Drop "arkhamPenalty") ArkhamPenalty

data ArkhamMatch
  = ArkhamMatchAt ArkhamLocationMatcher ArkhamMatch
  | ArkhamMatchAll [ArkhamMatch]
  | ArkhamMatchCardType ArkhamCardType
  | ArkhamMatchCardTrait ArkhamCardTrait
  | ArkhamMatchAlways
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via TaggedJson "match" ArkhamMatch

data ArkhamEffect
  = ArkhamEffectChooseOneOf [ArkhamEffect]
  | ArkhamEffectPenalty ArkhamPenalty
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via TaggedJson "effect" ArkhamEffect

data ArkhamChaosTokenMeaningValue
  = ArkhamChaosTokenMeaningValueStatic Int
  | ArkhamChaosTokenMeaningValueCount ArkhamMatch
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via TaggedJson "tokenMeaning" ArkhamChaosTokenMeaningValue

data ArkhamChaosTokenMeaning = ArkhamChaosTokenMeaning
  { arkhamChaosTokenMeaningValue :: ArkhamChaosTokenMeaningValue
  , arkhamChaosTokenMeaningOnFail :: [ArkhamEffect]
  , arkhamChaosTokenMeaningOnDraw :: [ArkhamEffect]
  }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via Codec (Drop "arkhamChaosTokenMeaning") ArkhamChaosTokenMeaning

data ArkhamTokenMeaningsDifficultiesJson = ArkhamTokenMeaningsDifficultiesJson
  { arkhamTokenMeaningsDifficultiesJsonEasy :: Map ArkhamChaosToken ArkhamChaosTokenMeaning
  , arkhamTokenMeaningsDifficultiesJsonStandard :: Map ArkhamChaosToken ArkhamChaosTokenMeaning
  , arkhamTokenMeaningsDifficultiesJsonHard :: Map ArkhamChaosToken ArkhamChaosTokenMeaning
  , arkhamTokenMeaningsDifficultiesJsonExpert :: Map ArkhamChaosToken ArkhamChaosTokenMeaning
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via Codec (Drop "arkhamTokenMeaningsDifficultiesJson") ArkhamTokenMeaningsDifficultiesJson


newtype ArkhamEncounterSetJson = ArkhamEncounterSetJson Text
  deriving stock (Generic, Show)
  deriving (PersistField, PersistFieldSql) via PersistJson ArkhamEncounterSetJson

instance ToJSON ArkhamEncounterSetJson where
  toJSON (ArkhamEncounterSetJson title) = object ["type" .= String "encounterSet", "id" .= String title]
  toEncoding (ArkhamEncounterSetJson title) = pairs ("type" .= ("encounterSet" :: Text) <> "id" .= title)

instance FromJSON ArkhamEncounterSetJson where
  parseJSON  = withObject "ArkhamEncounterSetJson" $ \v -> ArkhamEncounterSetJson <$> v .: "id"

data ArkhamScenarioJsonData = ArkhamScenarioJsonData
  { arkhamScenarioJsonDataFlavor :: Text
  , arkhamScenarioJsonDataEncounterSets :: [ArkhamEncounterSetJson]
  , arkhamScenarioJsonDataTokenMeanings :: ArkhamTokenMeaningsDifficultiesJson
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via Codec (Drop "arkhamScenarioJsonData") ArkhamScenarioJsonData
  deriving (PersistField, PersistFieldSql) via PersistJson ArkhamScenarioJsonData
