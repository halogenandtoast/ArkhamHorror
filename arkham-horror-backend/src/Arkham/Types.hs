module Arkham.Types where

import Prelude(Int, error, Show)
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
  deriving (FromJSON, ToJSON) via TaggedJson Stack

newtype ArkhamCycleStep = ArkhamCycleStepScenario ArkhamScenarioData
  deriving stock (Generic)
  deriving (ToJSON, FromJSON) via TaggedJson ArkhamCycleStep

data ArkhamScenarioData = ArkhamScenarioData
  { arkhamScenarioDataId :: Text
  , arkhamScenarioDataName :: Text
  }
  deriving stock (Generic)
  deriving (ToJSON, FromJSON) via Codec (Drop "arkhamScenarioData") ArkhamScenarioData

data ArkhamChaosTokenDifficulties = ArkhamChaosTokenDifficulties
  { arkhamChaosTokenDifficultiesEasy :: [ArkhamChaosToken]
  , arkhamChaosTokenDifficultiesStandard :: [ArkhamChaosToken]
  , arkhamChaosTokenDifficultiesHard :: [ArkhamChaosToken]
  , arkhamChaosTokenDifficultiesExpert :: [ArkhamChaosToken]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamChaosTokenDifficulties") ArkhamChaosTokenDifficulties

data ArkhamChaosToken = ArkhamChaosTokenNumber Int | ArkhamChaosTokenSkull | ArkhamChaosTokenHood | ArkhamChaosTokenStone | ArkhamChaosTokenTentacles | ArkhamChaosTokenElderSign
  deriving stock (Generic)
  deriving anyclass (FromJSON)

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



data ArkhamCycle = ArkhamCycle
  { cycleId :: Text
  , cycleName :: Text
  , cycleSteps :: [ArkhamCycleStep]
  , cycleChaosToken :: [ArkhamChaosTokenDifficulties]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "cycle") ArkhamCycle

data ArkhamScenario = ArkhamScenario
  { scenarioName :: Text
  , scenarioStacks :: [Stack]
  }
  deriving stock (Generic)
  deriving (ToJSON) via Codec (Drop "scenario") ArkhamScenario
