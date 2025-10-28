module Arkham.Act.Cards.EyesInTheTower (eyesInTheTower) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as ScarletKeys
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement

newtype EyesInTheTower = EyesInTheTower ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

eyesInTheTower :: ActCard EyesInTheTower
eyesInTheTower =
  act
    (3, A)
    EyesInTheTower
    Cards.eyesInTheTower
    (Just $ GroupClueCost (PerPlayer 3) "Tower of London")

instance RunMessage EyesInTheTower where
  runMessage msg a@(EyesInTheTower attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      towerPrison <- placeSetAsideLocation Locations.towerPrison
      createScarletKeyAt_ ScarletKeys.theEyeOfRavens (AttachedToLocation towerPrison)
      lead <- getLead
      allConcealedMiniCards >>= traverse_ (\c -> placeConcealedCard lead c (AtLocation towerPrison))
      selectEach (enemyIs Enemies.theRedGlovedManShroudedInMystery) removeEnemy
      theRedGlovedMan <- fetchCard Enemies.theRedGlovedManShroudedInMystery
      drawCard lead theRedGlovedMan
      advanceActDeck attrs
      pure a
    _ -> EyesInTheTower <$> liftRunMessage msg attrs
