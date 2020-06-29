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
import Arkham.Types.GameState
import Arkham.Types.Investigator
import Base.Lock
import ClassyPrelude
import Control.Monad.Random

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
  , scenarioSetup :: forall m. MonadRandom m => ArkhamGameState -> m ArkhamGameState
  , tokenMap :: HashMap ArkhamChaosToken ArkhamChaosTokenInternal
  , scenarioMythosPhase :: ArkhamMythosPhaseInternal
  , scenarioInvestigationPhase :: ArkhamInvestigationPhaseInternal
  , scenarioEnemyPhase :: ArkhamEnemyPhaseInternal
  , scenarioUpkeepPhase :: ArkhamUpkeepPhaseInternal
  , scenarioRun :: ArkhamGame -> ArkhamGame
  }

data ArkhamMythosPhaseInternal = ArkhamMythosPhaseInternal
  { mythosPhaseOnEnter :: Lockable ArkhamGame -> Lockable ArkhamGame
  , mythosPhaseAddDoom :: Lockable ArkhamGame -> Lockable ArkhamGame
  , mythosPhaseCheckAdvance :: Lockable ArkhamGame -> Lockable ArkhamGame
  , mythosPhaseDrawEncounter :: Lockable ArkhamGame -> Lockable ArkhamGame
  , mythosPhaseOnExit :: Lockable ArkhamGame -> Lockable ArkhamGame
  }

data ArkhamInvestigationPhaseInternal = ArkhamInvestigationPhaseInternal
  { investigationPhaseOnEnter :: Lockable ArkhamGame -> Lockable ArkhamGame
  , investigationPhaseTakeActions :: Lockable ArkhamGame -> Lockable ArkhamGame
  , investigationPhaseOnExit :: Lockable ArkhamGame -> Lockable ArkhamGame
  }

data ArkhamEnemyPhaseInternal = ArkhamEnemyPhaseInternal
  { enemyPhaseOnEnter :: Lockable ArkhamGame -> Lockable ArkhamGame
  , enemyPhaseResolveHunters :: Lockable ArkhamGame -> Lockable ArkhamGame
  , enemyPhaseResolveEnemies :: Lockable ArkhamGame -> Lockable ArkhamGame
  , enemyPhaseOnExit :: Lockable ArkhamGame -> Lockable ArkhamGame
  }

data ArkhamUpkeepPhaseInternal = ArkhamUpkeepPhaseInternal
  { upkeepPhaseOnEnter :: Lockable ArkhamGame -> Lockable ArkhamGame
  , upkeepPhaseResetActions :: Lockable ArkhamGame -> Lockable ArkhamGame
  , upkeepPhaseReadyExhausted :: Lockable ArkhamGame -> Lockable ArkhamGame
  , upkeepPhaseDrawCardsAndGainResources :: Lockable ArkhamGame -> Lockable ArkhamGame
  , upkeepPhaseCheckHandSize :: Lockable ArkhamGame -> Lockable ArkhamGame
  , upkeepPhaseOnExit :: Lockable ArkhamGame -> Lockable ArkhamGame
  }
