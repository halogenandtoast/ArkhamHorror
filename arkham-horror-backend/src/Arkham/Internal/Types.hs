module Arkham.Internal.Types where

import Arkham.Types
import ClassyPrelude

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

data ArkhamDifficulty = ArkhamEasy | ArkhamStandard | ArkhamHard | ArkhamExpert

data ArkhamScenarioInternal = ArkhamScenarioInternal
  { scenarioName :: Text
  , tokenMap :: HashMap ArkhamChaosToken ArkhamChaosTokenInternal
  }
