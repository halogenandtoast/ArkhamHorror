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

data ArkhamAgendaCard
  = ArkhamAgendaCardSideA ArkhamAgendaCardSideAData
  | ArkhamAgendaCardSideB ArkhamAgendaCardSideBData
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "agendaCard" ArkhamAgendaCard

data ArkhamAgendaCardSideAData = ArkhamAgendaCardSideAData
  { arkhamAgendaCardSideADataName :: Text
  , arkhamAgendaCardSideADataImageUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamAgendaCardSideAData") ArkhamAgendaCardSideAData

data ArkhamAgendaCardSideBData = ArkhamAgendaCardSideBData
  { arkhamAgendaCardSideBDataName :: Text
  , arkhamAgendaCardSideBDataImageUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamAgendaCardSideBData") ArkhamAgendaCardSideBData

newtype ArkhamAgenda = ArkhamAgenda { arkhamAgendaCurrentCard :: ArkhamAgendaCard }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamAgenda") ArkhamAgenda

data ArkhamActCard
  = ArkhamActCardSideA ArkhamActCardSideAData
  | ArkhamActCardSideB ArkhamActCardSideBData
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "actCard" ArkhamActCard

data ArkhamActCardSideAData = ArkhamActCardSideAData
  { arkhamActCardSideADataName :: Text
  , arkhamActCardSideADataImageUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamActCardSideAData") ArkhamActCardSideAData

data ArkhamActCardSideBData = ArkhamActCardSideBData
  { arkhamActCardSideBDataName :: Text
  , arkhamActCardSideBDataImageUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamActCardSideBData") ArkhamActCardSideBData

newtype ArkhamAct = ArkhamAct { arkhamActCurrentCard :: ArkhamActCard }
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
  { arkhamLocationUnrevealedDataId :: Text
  , arkhamLocationUnrevealedDataName :: Text
  , arkhamLocationUnrevealedDataSymbol :: ArkhamLocationSymbol
  , arkhamLocationUnrevealedDataImageUrl :: Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamLocationUnrevealedData") ArkhamLocationUnrevealedData

data ArkhamLocationRevealedData = ArkhamLocationRevealedData
  { arkhamLocationRevealedDataId :: Text
  , arkhamLocationRevealedDataName :: Text
  , arkhamLocationRevealedDataSymbol :: ArkhamLocationSymbol
  , arkhamLocationRevealedDataConnections :: [ArkhamLocationSymbol]
  , arkhamLocationRevealedDataShroud :: Int
  , arkhamLocationRevealedDataMaxClues :: ArkhamClueCount
  , arkhamLocationRevealedDataCurrentClues :: Int
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
  , scenarioStacks :: [ArkhamStack] -- Should we call these decks
  , scenarioLocations :: [ArkhamLocation]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "scenario") ArkhamScenario

data ArkhamSkill
  = ArkhamSkillWillpower
  | ArkhamSkillCombat
  | ArkhamSkillIntellect
  | ArkhamSkillAgility
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "skill" ArkhamSkill

data ArkhamSkillCheckTarget
  = ArkhamSkillCheckTargetLocation ArkhamLocation
  | ArkhamSkillCheckTargetMythosCard
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "target" ArkhamSkillCheckTarget

data ArkhamSkillCheck = ArkhamSkillCheck
  { arkhamSkillCheckBase :: Int
  , arkhamSkillCheckSkill :: ArkhamSkill
  , arkhamSkillCheckTarget :: ArkhamSkillCheckTarget
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamSkillCheck") ArkhamSkillCheck

data ArkhamSkillCheckResult = ArkhamSkillCheckResult
  { arkhamSkillCheckResultToken :: ArkhamChaosToken
  , arkhamSkillCheckResultBase :: Int
  , arkhamSkillCheckResultSkill :: ArkhamSkill
  , arkhamSkillCheckResultTarget :: ArkhamSkillCheckTarget
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamSkillCheckResult") ArkhamSkillCheckResult
