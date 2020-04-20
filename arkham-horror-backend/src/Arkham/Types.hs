{-# LANGUAGE OverloadedStrings #-}
module Arkham.Types where

import Prelude(Int, error, Show, (++), ($), fail, pure, (<$>))
import Json
import GHC.Generics
import Data.Text

newtype ArkhamCardFront = ArkhamCardFront { frontUrl :: Text }
  deriving stock (Show,Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "front") ArkhamCardFront

newtype ArkhamCardBack = ArkhamCardBack { backUrl :: Text }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "back") ArkhamCardBack

data ArkhamCard = ArkhamCard
  { cardFront :: ArkhamCardFront
  , cardBack :: ArkhamCardBack
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "card") ArkhamCard

newtype Agenda = Agenda { agendaCurrentCard :: ArkhamCard }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "agenda") Agenda

newtype Act = Act { actCurrentCard :: ArkhamCard }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "act") Act

data Stack = StackAgenda Agenda | StackAct Act
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via TaggedJson "stack" Stack

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
  toJSON (ArkhamChaosTokenNumber x) = object [ "tag" .= String "token", "type" .= String "number", "value" .= toJSON x ]
  toJSON other = object [ "tag" .= String "token", "type" .= String tokenType ]
    where
      tokenType :: Text
      tokenType
        = case other of
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

data ArkhamLocationSymbol = Circle | Square | Heart
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamAction = ArkhamActionRevealLocation Int | ArkhamActionInvestigate Int
  deriving stock (Generic)

instance ToJSON ArkhamAction where
  toJSON (ArkhamActionRevealLocation n) = object [ "tag" .= String "action", "type" .= String "revealLocation", "target" .= n ]
  toJSON (ArkhamActionInvestigate n) = object [ "tag" .= String "action", "type" .= String "investigate", "target" .= n ]

data ArkhamLocationFront = ArkhamLocationFront
  { arkhamLocationFrontName :: Text
  , arkhamLocationFrontSymbol :: ArkhamLocationSymbol
  , arkhamLocationFrontCard :: ArkhamCardFront
  }
  deriving stock (Generic)
  deriving (ToJSON) via Codec (Drop "arkhamLocationFront") ArkhamLocationFront

data ArkhamClueCount = ArkhamClueCountNumber Int | ArkhamClueCountPerInvestigator Int
  deriving stock (Generic)
  deriving (ToJSON) via TaggedJson "ArkhamClueCount" ArkhamClueCount

data ArkhamLocationBack = ArkhamLocationBack
  { arkhamLocationBackName :: Text
  , arkhamLocationBackSymbol :: ArkhamLocationSymbol
  , arkhamLocationBackConnections :: [ArkhamLocationSymbol]
  , arkhamLocationBackCard :: ArkhamCardBack
  , arkhamLocationBackShroud :: Int
  , arkhamLocationBackClues :: ArkhamClueCount
  }
  deriving stock (Generic)
  deriving (ToJSON) via Codec (Drop "arkhamLocationBack") ArkhamLocationBack

data ArkhamLocation = ArkhamLocation
  { arkhamLocationFront :: ArkhamLocationFront
  , arkhamLocationBack :: ArkhamLocationBack
  }
  deriving stock (Generic)
  deriving (ToJSON) via Codec (Drop "arkhamLocation") ArkhamLocation


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
  , scenarioStacks :: [Stack]
  , scenarioLocations :: [ArkhamLocation]
  }
  deriving stock (Generic)
  deriving (ToJSON) via Codec (Drop "scenario") ArkhamScenario
