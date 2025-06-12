module Arkham.Act.Cards.AtTheExhibitTheBrotherhoodsPlot (atTheExhibitTheBrotherhoodsPlot) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype AtTheExhibitTheBrotherhoodsPlot = AtTheExhibitTheBrotherhoodsPlot ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheExhibitTheBrotherhoodsPlot :: ActCard AtTheExhibitTheBrotherhoodsPlot
atTheExhibitTheBrotherhoodsPlot =
  act (2, A) AtTheExhibitTheBrotherhoodsPlot Cards.atTheExhibitTheBrotherhoodsPlot
    $ Just
    $ GroupClueCost (PerPlayer 2) "Eztli Exhibit"

instance RunMessage AtTheExhibitTheBrotherhoodsPlot where
  runMessage msg a@(AtTheExhibitTheBrotherhoodsPlot attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      findEncounterCardIn lead attrs (cardIs Enemies.brotherhoodCultist) [#deck, #discard, #victory]
      doStep 1 msg
      advanceToAct attrs Acts.recoverTheRelic A
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      brotherhoodCultists <- select $ enemyIs Enemies.brotherhoodCultist
      farthestBrotherhoodCultists <- select $ FarthestEnemyFromAll $ enemyIs Enemies.brotherhoodCultist
      deckCount <- getActDecksInPlayCount
      for_ brotherhoodCultists (healAllDamage attrs)

      leadChooseOrRunOneM do
        targets farthestBrotherhoodCultists \cultist -> do
          createAssetAt_ Assets.relicOfAgesADeviceOfSomeSort (AttachedToEnemy cultist)
          when (deckCount <= 2) $ placeDoom attrs cultist 1
      pure a
    FoundEncounterCard _ target card | isTarget attrs target -> do
      locations <- select $ FarthestLocationFromAll Anywhere
      leadChooseOrRunOneM $ targets locations (spawnEnemyAt_ card)
      pure a
    _ -> AtTheExhibitTheBrotherhoodsPlot <$> liftRunMessage msg attrs
