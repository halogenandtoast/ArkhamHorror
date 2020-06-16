module Arkham.Types
  ( module X
  , module Arkham.Types
  )
where

import Arkham.Types.ChaosToken as X
import Arkham.Types.Location as X
import ClassyPrelude
import Control.Monad.Random
import Data.Aeson
import Data.Aeson.Casing
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

data ArkhamCycle = NightOfTheZealot | TheDunwichLegacy
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamScenario = ScenarioOne | ScenarioTwo
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamInvestigator = ArkhamInvestigator
  { investigatorName :: Text
  , investigatorImage :: Text
  , investigatorPortrait :: Text
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
  , clues :: Int
  , hand :: [ArkhamCard]
  , inPlay :: [ArkhamCard]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamPhase = Mythos | Investigation | Enemy | Upkeep
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamGameState = ArkhamGameState
  { agsPlayer :: ArkhamPlayer
  , agsPhase :: ArkhamPhase
  , agsChaosBag :: NonEmpty ArkhamChaosToken
  , agsLocations :: [ArkhamLocation]
  }
  deriving stock (Generic)

instance ToJSON ArkhamGameState where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

class HasChaosBag a where
  chaosBag :: a -> NonEmpty ArkhamChaosToken
  drawFromChaosBag :: (MonadRandom m) => a -> m ArkhamChaosToken
  drawFromChaosBag a = let bag = chaosBag a in (bag NE.!!) <$> getRandomR (0, NE.length bag - 1)

data ArkhamGame = ArkhamGame
  { agId :: Int
  , agCycle :: ArkhamCycle
  , agScenario :: ArkhamScenario
  , agGameState :: ArkhamGameState
  }
  deriving stock (Generic)

instance HasChaosBag ArkhamGame where
  chaosBag = agsChaosBag . agGameState

instance ToJSON ArkhamGame where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
