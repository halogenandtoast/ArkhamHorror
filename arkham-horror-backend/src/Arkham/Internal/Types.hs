module Arkham.Internal.Types where

import Arkham.Types
import ClassyPrelude

data Lockable b a = Locked (b -> Bool) a | Unlocked a

applyLock :: b -> (a -> Lockable b a) -> Lockable b a -> Lockable b a
applyLock _ f (Unlocked a) = f a
applyLock key f (Locked lock a) | lock key = f a
applyLock _ _ l = l

removeLock :: Lockable b a -> a
removeLock (Locked _ a) = a
removeLock (Unlocked a) = a

isLocked :: Lockable b a -> Bool
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
