module Arkham.Act.Cards.ExploringPnakotus (exploringPnakotus) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Data.List.NonEmpty qualified as NE

newtype ExploringPnakotus = ExploringPnakotus ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

exploringPnakotus :: ActCard ExploringPnakotus
exploringPnakotus =
  act
    (1, A)
    ExploringPnakotus
    Cards.exploringPnakotus
    (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance HasModifiersFor ExploringPnakotus where
  getModifiersFor (ExploringPnakotus attrs) =
    when (onSide A attrs) do
      modifySelectWith attrs (enemyIs Enemies.yithianObserver) setActiveDuringSetup [AddKeyword Aloof]

instance RunMessage ExploringPnakotus where
  runMessage msg a@(ExploringPnakotus attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (enemyIs Enemies.yithianObserver) enemyCheckEngagement
      locations <- getSetAsideCardsMatching LocationCard >>= traverse placeLocation
      spawnLocation <- maybe (error "no locations") sample $ NE.nonEmpty locations
      custodian <- fetchCard Assets.theCustodian
      createAssetAt_ custodian $ AtLocation spawnLocation
      advanceActDeck attrs
      pure a
    _ -> ExploringPnakotus <$> liftRunMessage msg attrs
