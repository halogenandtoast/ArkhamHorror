module Arkham.Internal.Types
  ( ArkhamChaosTokenInternal(..)
  , ArkhamInvestigatorInternal(..)
  , ArkhamScenarioInternal(..)
  , ArkhamChaosTokenResult(..)
  , ArkhamActionType(..)
  , ArkhamUpkeepPhaseInternal(..)
  , ArkhamEnemyPhaseInternal(..)
  , ArkhamMythosPhaseInternal(..)
  , ArkhamInvestigationPhaseInternal(..)
  , ArkhamCardTrait(..)
  )
where

import Arkham.Entity.ArkhamGame
import Arkham.Types.ChaosToken
import Arkham.Types.Game
import Arkham.Types.GameState
import Arkham.Types.Investigator
import Base.Lock
import ClassyPrelude
import Lens.Micro

data ArkhamCardTrait = Tome | Ghoul

data ArkhamActionType = AnyAction | TraitRestrictedAction ArkhamCardTrait

-- TODO: Should this be a reader with access to the investigator... probably
data ArkhamInvestigatorInternal = ArkhamInvestigatorInternal
  { investigatorElderSignToken :: ArkhamChaosTokenInternal
  , investigatorOnDefeatEnemy :: ArkhamGameState -> ArkhamGameState
  , investigatorAvailableActions :: [ArkhamActionType]
  }

data ArkhamChaosTokenResult = Modifier Int | Failure

data ArkhamChaosTokenInternal = ArkhamChaosTokenInternal
  { tokenToResult :: ArkhamGameState -> ArkhamInvestigator -> ArkhamChaosTokenResult
  , tokenOnFail :: ArkhamGameState -> ArkhamInvestigator -> ArkhamGameState
  , tokenOnSuccess :: ArkhamGameState -> ArkhamInvestigator -> ArkhamGameState
  , tokenOnReveal :: ArkhamGameState -> ArkhamInvestigator -> ArkhamGameState
  }

data ArkhamScenarioInternal = ArkhamScenarioInternal
  { scenarioName :: Text
  , scenarioSetup :: ArkhamGameState -> ArkhamGameState
  , tokenMap :: HashMap ArkhamChaosToken ArkhamChaosTokenInternal
  , scenarioMythosPhase :: ArkhamMythosPhaseInternal
  , scenarioInvestigationPhase :: ArkhamInvestigationPhaseInternal
  , scenarioEnemyPhase :: ArkhamEnemyPhaseInternal
  , scenarioUpkeepPhase :: ArkhamUpkeepPhaseInternal
  , scenarioRun :: ArkhamGame -> ArkhamGame
  }

data ArkhamMythosPhaseInternal = ArkhamMythosPhaseInternal
  { mythosPhaseOnEnter :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , mythosPhaseAddDoom :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , mythosPhaseCheckAdvance :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , mythosPhaseDrawEncounter :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , mythosPhaseOnExit :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  }

data ArkhamInvestigationPhaseInternal = ArkhamInvestigationPhaseInternal
  { investigationPhaseOnEnter :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , investigationPhaseTakeActions :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , investigationPhaseOnExit :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  }

data ArkhamEnemyPhaseInternal = ArkhamEnemyPhaseInternal
  { enemyPhaseOnEnter :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , enemyPhaseResolveHunters :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , enemyPhaseResolveEnemies :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , enemyPhaseOnExit :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  }

data ArkhamUpkeepPhaseInternal = ArkhamUpkeepPhaseInternal
  { upkeepPhaseOnEnter :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , upkeepPhaseResetActions :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , upkeepPhaseReadyExhausted :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , upkeepPhaseDrawCardsAndGainResources :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , upkeepPhaseCheckHandSize :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , upkeepPhaseOnExit :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  }
