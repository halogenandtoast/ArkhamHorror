module Arkham.Types
  ( module X
  , module Arkham.Types
  )
where

import Arkham.Entity.ArkhamGame as X
import Arkham.Types.Action as X
import Arkham.Types.Card as X
import Arkham.Types.ChaosToken as X
import Arkham.Types.Game as X
import Arkham.Types.GameState as X
import Arkham.Types.Investigator as X
import Arkham.Types.Location as X
import Arkham.Types.Player as X
import Arkham.Types.Scenario as X
import Arkham.Types.Skill as X
import ClassyPrelude
import Control.Monad.Random
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Lens.Micro

gameState :: Lens' ArkhamGameData ArkhamGameState
gameState = lens agGameState $ \m x -> m { agGameState = x }

class HasChaosBag a where
  chaosBag :: Lens' a (NonEmpty ArkhamChaosToken)
  drawChaosToken :: (MonadRandom m) => a -> m ArkhamChaosToken
  drawChaosToken a = let bag = a ^. chaosBag in (bag NE.!!) <$> getRandomR (0, NE.length bag - 1)

instance HasChaosBag ArkhamGameState where
  chaosBag = lens agsChaosBag $ \m x -> m { agsChaosBag = x }

instance HasChaosBag ArkhamGameData where
  chaosBag = gameState . chaosBag

instance HasChaosBag ArkhamGame where
  chaosBag = currentData . chaosBag

class HasLocations a where
  locations :: Lens' a [ArkhamLocation]

instance HasLocations ArkhamGameState where
  locations = lens agsLocations $ \m x -> m { agsLocations = x }

instance HasLocations ArkhamGameData where
  locations = gameState . locations

class HasLocationId a where
  locationId :: Lens' a LocationId

instance HasLocationId ArkhamUnrevealedLocation where
  locationId = lens aulLocationId $ \m x -> m { aulLocationId = x }

instance HasLocationId ArkhamRevealedLocation where
  locationId = lens arlLocationId $ \m x -> m { arlLocationId = x }

instance HasLocationId ArkhamLocation where
  locationId = lens (^. locationId) $ \m x -> m & locationId .~ x

class HasCurrentData a where
  currentData :: Lens' a ArkhamGameData

instance HasCurrentData ArkhamGame where
  currentData =
    lens arkhamGameCurrentData (\m x -> m { arkhamGameCurrentData = x })

class HasGameStateStep a where
  gameStateStep :: Lens' a ArkhamGameStateStep

instance HasGameStateStep ArkhamGame where
  gameStateStep = currentData . gameStateStep

instance HasGameStateStep ArkhamGameData where
  gameStateStep = gameState . gameStateStep

instance HasGameStateStep ArkhamGameState where
  gameStateStep = lens agsStep $ \m x -> m { agsStep = x }

class HasPlayer a where
  player :: Lens' a ArkhamPlayer

instance HasPlayer ArkhamGameData where
  player = gameState . player

instance HasPlayer ArkhamGameState where
  player = lens agsPlayer $ \m x -> m { agsPlayer = x }

class HasResources a where
  resources :: Lens' a Int

instance HasResources ArkhamPlayer where
  resources = lens _resources $ \m x -> m { _resources = x }
