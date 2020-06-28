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

removeLock :: (HasLock a, b ~ LockKey a) => Lockable b a -> a
removeLock (Locked _ a) = a & lock .~ Nothing
removeLock (Unlocked a) = a & lock .~ Nothing

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
  , tokenMap :: HashMap ArkhamChaosToken ArkhamChaosTokenInternal
  , scenarioMythosPhase :: ArkhamMythosPhaseInternal
  , scenarioInvestigationPhase :: ArkhamInvestigationPhaseInternal
  }

data ArkhamMythosPhaseInternal = ArkhamMythosPhaseInternal
  { mythosAddDoom :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , mythosCheckAdvance :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , mythosDrawEncounter :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , mythosOnEnd :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  }

data ArkhamInvestigationPhaseInternal = ArkhamInvestigationPhaseInternal
  { investigationPhaseOnEnter :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , investigationPhaseTakeActions :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  , investigationPhaseOnExit :: Lockable String ArkhamGame -> Lockable String ArkhamGame
  }
