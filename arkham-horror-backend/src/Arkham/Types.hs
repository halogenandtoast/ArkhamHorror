module Arkham.Types where

import Data.Text
import GHC.Generics
import Json
import Prelude (Int, Show)

data ArkhamInvestigator = ArkhamInvestigator
  { arkhamInvestigatorName :: Text
  , arkhamInvestigatorWillpower :: Int
  , arkhamInvestigatorIntellect :: Int
  , arkhamInvestigatorCombat :: Int
  , arkhamInvestigatorAgility :: Int
  , arkhamInvestigatorHealth :: Int
  , arkhamInvestigatorSanity :: Int
  , arkhamInvestigatorFrontImageUrl :: Text
  , arkhamInvestigatorBackImageUrl :: Text
  }
  deriving stock (Show,Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamInvestigator") ArkhamInvestigator

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
  deriving (ToJSON, FromJSON) via TaggedJson "token" ArkhamChaosToken

data ArkhamLocationSymbol = ArkhamLocationSymbolCircle | ArkhamLocationSymbolSquare | ArkhamLocationSymbolHeart
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via TaggedJson "symbol" ArkhamLocationSymbol

data ArkhamAction = ArkhamActionRevealLocation Int | ArkhamActionInvestigate Int
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "action" ArkhamAction

data ArkhamClueCount = ArkhamClueCountNumber Int | ArkhamClueCountPerInvestigator Int
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "clueCount" ArkhamClueCount

data ArkhamLocationUnrevealedData = ArkhamLocationUnrevealedData
  { arkhamLocationUnrevealedDataName :: Text
  , arkhamLocationUnrevealedDataSymbol :: ArkhamLocationSymbol
  , arkhamLocationUnrevealedDataImageUrl :: Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamLocationUnrevealedData") ArkhamLocationUnrevealedData

data ArkhamLocationRevealedData = ArkhamLocationRevealedData
  { arkhamLocationRevealedDataName :: Text
  , arkhamLocationRevealedDataSymbol :: ArkhamLocationSymbol
  , arkhamLocationRevealedDataConnections :: [ArkhamLocationSymbol]
  , arkhamLocationRevealedDataShroud :: Int
  , arkhamLocationRevealedDataClues :: ArkhamClueCount
  , arkhamLocationRevealedDataImageUrl :: Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamLocationRevealedData") ArkhamLocationRevealedData

data ArkhamLocation = ArkhamLocationUnrevealed ArkhamLocationUnrevealedData | ArkhamLocationRevealed ArkhamLocationRevealedData
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "location" ArkhamLocation

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
