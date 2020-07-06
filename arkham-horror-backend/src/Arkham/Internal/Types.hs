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
  )
where

import Arkham.Entity.ArkhamGame
import Arkham.Types.Card
import Arkham.Types.ChaosToken
import Arkham.Types.GameState
import Arkham.Types.Player
import Arkham.Types.Trait
import Base.Lock
import ClassyPrelude
import Control.Monad.Random

data ArkhamActionType = AnyAction | TraitRestrictedAction ArkhamTrait

-- TODO: Should this be a reader with access to the investigator... probably
data ArkhamInvestigatorInternal = ArkhamInvestigatorInternal
  { investigatorElderSignToken :: ArkhamChaosTokenInternal
  , investigatorOnDefeatEnemy :: ArkhamGameState -> ArkhamGameState
  , investigatorAvailableActions :: [ArkhamActionType]
  }

data ArkhamChaosTokenResult = Modifier Int | Failure

data ArkhamChaosTokenInternal = ArkhamChaosTokenInternal
  { tokenToResult :: ArkhamGameState -> ArkhamPlayer -> ArkhamChaosTokenResult
  , tokenOnFail :: ArkhamGameState -> ArkhamPlayer -> ArkhamGameState
  , tokenOnSuccess :: ArkhamGameState -> ArkhamPlayer -> ArkhamGameState
  , tokenOnReveal :: ArkhamGameState -> ArkhamPlayer -> ArkhamGameState
  }

data ArkhamScenarioInternal = ArkhamScenarioInternal
  { scenarioName :: Text
  , scenarioSetup :: forall m. MonadRandom m => ArkhamGameState -> m ArkhamGameState
  , scenarioTokenMap :: HashMap ArkhamChaosToken ArkhamChaosTokenInternal
  , scenarioUpdateObjectives :: PhaseStep
  , scenarioUpdateAccessibleLocationsOnPlayers :: PhaseStep
  , scenarioMythosPhase :: ArkhamMythosPhaseInternal
  , scenarioInvestigationPhase :: ArkhamInvestigationPhaseInternal
  , scenarioEnemyPhase :: ArkhamEnemyPhaseInternal
  , scenarioUpkeepPhase :: ArkhamUpkeepPhaseInternal
  , scenarioRun :: ArkhamGame -> IO ArkhamGame
  , scenarioFindAct :: ArkhamCardCode -> ArkhamGame -> ArkhamAct
  , scenarioLocationGraph :: ArkhamGame -> ArkhamCardCode -> [ArkhamCardCode]
  }

data ArkhamMythosPhaseInternal = ArkhamMythosPhaseInternal
  { mythosPhaseOnEnter :: PhaseStep
  , mythosPhaseAddDoom :: PhaseStep
  , mythosPhaseCheckAdvance :: PhaseStep
  , mythosPhaseDrawEncounter :: PhaseStep
  , mythosPhaseOnExit :: PhaseStep
  }

type PhaseStep
  = forall m . MonadIO m => Lockable ArkhamGame -> m (Lockable ArkhamGame)

data ArkhamInvestigationPhaseInternal = ArkhamInvestigationPhaseInternal
  { investigationPhaseOnEnter :: PhaseStep
  , investigationPhaseTakeActions :: PhaseStep
  , investigationPhaseOnExit :: PhaseStep
  }

data ArkhamEnemyPhaseInternal = ArkhamEnemyPhaseInternal
  { enemyPhaseOnEnter :: PhaseStep
  , enemyPhaseResolveHunters :: PhaseStep
  , enemyPhaseResolveEnemies :: PhaseStep
  , enemyPhaseOnExit :: PhaseStep
  }

data ArkhamUpkeepPhaseInternal = ArkhamUpkeepPhaseInternal
  { upkeepPhaseOnEnter :: PhaseStep
  , upkeepPhaseResetActions :: PhaseStep
  , upkeepPhaseReadyExhausted :: PhaseStep
  , upkeepPhaseDrawCardsAndGainResources :: PhaseStep
  , upkeepPhaseCheckHandSize :: PhaseStep
  , upkeepPhaseOnExit :: PhaseStep
  }
