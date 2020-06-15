module Arkham.Types where

import ClassyPrelude
import Data.Aeson
import Data.List.NonEmpty

data ArkhamChaosToken
  = PlusOne
  | Zero
  | MinusOne
  | MinusTwo
  | MinusThree
  | MinusFour
  | MinusFive
  | MinusSix
  | MinusSeven
  | MinusEight
  | Skull
  | Cultist
  | Tablet
  | ElderThing
  | Fail
  | ElderSign
  deriving stock (Generic)
  deriving anyclass (ToJSON)


data ArkhamCycle = NightOfTheZealot | TheDunwichLegacy
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamScenario = ScenarioOne | ScenarioTwo
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamInvestigator = ArkhamInvestigator
  { investigatorName :: Text
  , investigatorImage :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamCard = ArkhamCard
  { cost :: Maybe Int
  , image :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamPlayer = ArkhamPlayer
  { investigator :: ArkhamInvestigator
  , sanityDamage :: Int
  , healthDamage :: Int
  , resources :: Int
  , hand :: [ArkhamCard]
  , inPlay :: [ArkhamCard]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamPhase = Mythos | Investigation | Enemy | Upkeep
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamGameState = ArkhamGameState
  { player :: ArkhamPlayer
  , phase :: ArkhamPhase
  , chaosBag :: NonEmpty ArkhamChaosToken
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamGame = ArkhamGame
  { cycle :: ArkhamCycle
  , scenario :: ArkhamScenario
  , gameState :: ArkhamGameState
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
