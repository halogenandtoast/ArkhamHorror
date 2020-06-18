module Arkham.Types
  ( module X
  , module Arkham.Types
  )
where

import Arkham.Types.Card as X
import Arkham.Types.ChaosToken as X
import Arkham.Types.Game as X
import Arkham.Types.GameState as X
import Arkham.Types.Investigator as X
import Arkham.Types.Location as X
import Arkham.Types.Player as X
import Arkham.Types.Scenario as X
import ClassyPrelude
import Control.Monad.Random
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Lens.Micro

class HasChaosBag a where
  chaosBag :: Lens' a (NonEmpty ArkhamChaosToken)
  drawFromChaosBag :: (MonadRandom m) => a -> m ArkhamChaosToken
  drawFromChaosBag a = let bag = a ^. chaosBag in (bag NE.!!) <$> getRandomR (0, NE.length bag - 1)

arkhamGameGameStateLens :: Lens' ArkhamGame ArkhamGameState
arkhamGameGameStateLens = lens agGameState $ \m x -> m { agGameState = x }

arkhamGameStateChaosBagLens :: Lens' ArkhamGameState (NonEmpty ArkhamChaosToken)
arkhamGameStateChaosBagLens = lens agsChaosBag $ \m x -> m { agsChaosBag = x }

arkhamGameChaosBagLens :: Lens' ArkhamGame (NonEmpty ArkhamChaosToken)
arkhamGameChaosBagLens = arkhamGameGameStateLens . arkhamGameStateChaosBagLens

instance HasChaosBag ArkhamGameState where
  chaosBag = arkhamGameStateChaosBagLens

instance HasChaosBag ArkhamGame where
  chaosBag = arkhamGameChaosBagLens
