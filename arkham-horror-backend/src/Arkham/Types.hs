{-# LANGUAGE OverloadedStrings #-}
module Arkham.Types where

import Data.Text
import GHC.Generics
import Json
import Prelude (Int, Show, error, fail, pure, ($), (++), (<$>))

newtype ArkhamCardFront = ArkhamCardFront { arkhamCardFrontUrl :: Text }
  deriving stock (Show,Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamCardFront") ArkhamCardFront

newtype ArkhamCardBack = ArkhamCardBack { arkhamCardBackUrl :: Text }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamCardBack") ArkhamCardBack

data ArkhamCard = ArkhamCard
  { arkhamCardFront :: ArkhamCardFront
  , arkhamCardBack :: ArkhamCardBack
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamCard") ArkhamCard

newtype ArkhamAgenda = ArkhamAgenda { arkhamAgendaCurrentCard :: ArkhamCard }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamAgenda") ArkhamAgenda

newtype ArkhamAct = ArkhamAct { arkhamActCurrentCard :: ArkhamCard }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamAct") ArkhamAct

data ArkhamStack = ArkhamStackAgenda ArkhamAgenda | ArkhamStackAct ArkhamAct
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via TaggedJson "stack" ArkhamStack

newtype ArkhamCycleStep = ArkhamCycleStepScenario ArkhamScenarioData
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via TaggedJson "step" ArkhamCycleStep

data ArkhamScenarioData = ArkhamScenarioData
  { arkhamScenarioId :: Text
  , arkhamScenarioName :: Text
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via Codec (Drop "arkham") ArkhamScenarioData

data ArkhamChaosTokenDifficulties = ArkhamChaosTokenDifficulties
  { arkhamChaosTokenDifficultiesEasy :: [ArkhamChaosToken]
  , arkhamChaosTokenDifficultiesStandard :: [ArkhamChaosToken]
  , arkhamChaosTokenDifficultiesHard :: [ArkhamChaosToken]
  , arkhamChaosTokenDifficultiesExpert :: [ArkhamChaosToken]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamChaosTokenDifficulties") ArkhamChaosTokenDifficulties

data ArkhamChaosToken = ArkhamChaosTokenNumber Int  | ArkhamChaosTokenSkull | ArkhamChaosTokenHood | ArkhamChaosTokenStone | ArkhamChaosTokenTentacles | ArkhamChaosTokenElderSign
  deriving stock (Generic, Show)

instance ToJSON ArkhamChaosToken where
  toJSON (ArkhamChaosTokenNumber x) = object
    ["tag" .= String "token", "type" .= String "number", "value" .= toJSON x]
  toJSON other = object ["tag" .= String "token", "type" .= String tokenType]
   where
    tokenType :: Text
    tokenType = case other of
      ArkhamChaosTokenSkull -> "skull"
      ArkhamChaosTokenHood -> "hood"
      ArkhamChaosTokenStone -> "stone"
      ArkhamChaosTokenTentacles -> "tentacles"
      ArkhamChaosTokenElderSign -> "elderSign"
      _ -> error "impossible"

instance FromJSON ArkhamChaosToken where
  parseJSON = withObject "ArkhamChaosToken" $ \v -> do
    tokenType <- v .: "type"
    case tokenType of
      "number" -> ArkhamChaosTokenNumber <$> v .: "value"
      "skull" -> pure ArkhamChaosTokenSkull
      "hood" -> pure ArkhamChaosTokenHood
      "stone" -> pure ArkhamChaosTokenStone
      "tentacles" -> pure ArkhamChaosTokenTentacles
      "elderSign" -> pure ArkhamChaosTokenElderSign
      _ -> fail $ tokenType ++ " is not a valid token type"

data ArkhamLocationSymbol = ArkhamLocationSymbolCircle | ArkhamLocationSymbolSquare | ArkhamLocationSymbolHeart
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via TaggedJson "symbol" ArkhamLocationSymbol

data ArkhamAction = ArkhamActionRevealLocation Int | ArkhamActionInvestigate Int
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "action" ArkhamAction

data ArkhamLocationFront = ArkhamLocationFront
  { arkhamLocationFrontName :: Text
  , arkhamLocationFrontSymbol :: ArkhamLocationSymbol
  , arkhamLocationFrontCard :: ArkhamCardFront
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamLocationFront") ArkhamLocationFront

data ArkhamClueCount = ArkhamClueCountNumber Int | ArkhamClueCountPerInvestigator Int
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "clueCount" ArkhamClueCount

data ArkhamLocationBack = ArkhamLocationBack
  { arkhamLocationBackName :: Text
  , arkhamLocationBackSymbol :: ArkhamLocationSymbol
  , arkhamLocationBackConnections :: [ArkhamLocationSymbol]
  , arkhamLocationBackCard :: ArkhamCardBack
  , arkhamLocationBackShroud :: Int
  , arkhamLocationBackClues :: ArkhamClueCount
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamLocationBack") ArkhamLocationBack

data ArkhamLocation = ArkhamLocation
  { arkhamLocationFront :: ArkhamLocationFront
  , arkhamLocationBack :: ArkhamLocationBack
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamLocation") ArkhamLocation


data ArkhamCycle = ArkhamCycle
  { cycleId :: Text
  , cycleName :: Text
  , cycleChaosTokens :: ArkhamChaosTokenDifficulties
  , cycleSteps :: [ArkhamCycleStep]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "cycle") ArkhamCycle

data ArkhamScenario = ArkhamScenario
  { scenarioName :: Text
  , scenarioStacks :: [ArkhamStack]
  , scenarioLocations :: [ArkhamLocation]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "scenario") ArkhamScenario
