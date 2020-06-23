module Arkham.Internal.Types where

import Arkham.Types
import ClassyPrelude
import Lens.Micro

class HasLock a where
  type LockKey a
  lock :: Lens' a (Maybe (LockKey a))

data Lockable b a = Locked (b -> Bool) a | Unlocked a

instance HasLock ArkhamGame where
  type LockKey ArkhamGame = String
  lock = currentData . lock

instance HasLock ArkhamGameData where
  type LockKey ArkhamGameData = String
  lock = gameState . lock

instance HasLock ArkhamGameState where
  type LockKey ArkhamGameState = String
  lock = lens agsLock $ \m x -> m { agsLock = x }

buildLock :: (HasLock a, b ~ LockKey a, Eq b) => a -> Lockable b a
buildLock a = case a ^. lock of
  Just lock' -> Locked (== lock') a
  Nothing -> Unlocked a

addLock :: (HasLock a, b ~ LockKey a, Eq b) => b -> a -> Lockable b a
addLock b a = Locked (== b) $ a & lock ?~ b

runLocked
  :: (HasLock a, b ~ LockKey a)
  => b
  -> (a -> Lockable b a)
  -> Lockable b a
  -> Lockable b a
runLocked _ f (Unlocked a) = f a
runLocked key f (Locked lock' a) | lock' key = f a
runLocked _ _ l = l

runOnlyLocked
  :: (HasLock a, b ~ LockKey a)
  => b
  -> (a -> Lockable b a)
  -> Lockable b a
  -> Lockable b a
runOnlyLocked key f (Locked lock' a) | lock' key = f a
runOnlyLocked _ _ l = l

removeLock :: (HasLock a, b ~ LockKey a) => Lockable b a -> a
removeLock a = withoutLock a & lock .~ Nothing

withoutLock :: (HasLock a, b ~ LockKey a) => Lockable b a -> a
withoutLock (Locked _ a) = a
withoutLock (Unlocked a) = a

isLocked :: (HasLock a, b ~ LockKey a) => Lockable b a -> Bool
isLocked (Locked _ _) = True
isLocked (Unlocked _) = False

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
