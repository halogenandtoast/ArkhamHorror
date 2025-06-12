module Arkham.Act.Cards.AtTheStationInShadowedTalons (atTheStationInShadowedTalons) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype AtTheStationInShadowedTalons = AtTheStationInShadowedTalons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheStationInShadowedTalons :: ActCard AtTheStationInShadowedTalons
atTheStationInShadowedTalons =
  act (2, C) AtTheStationInShadowedTalons Cards.atTheStationInShadowedTalons
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ LocationWithTitle "Arkham Police Station"

instance RunMessage AtTheStationInShadowedTalons where
  runMessage msg a@(AtTheStationInShadowedTalons attrs) = runQueueT $ case msg of
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      lead <- getLead
      findEncounterCardIn lead attrs (cardIs Enemies.huntingNightgaunt) [#deck, #discard, #victory]
      doStep 1 msg
      advanceToAct attrs Acts.alejandrosPlight C
      pure a
    DoStep 1 (AdvanceAct (isSide D attrs -> True) _ _) -> do
      selectEach (enemyIs Enemies.huntingNightgaunt) (healAllDamage attrs)
      farthestHuntingNightGaunts <- select $ FarthestEnemyFromAll $ enemyIs Enemies.huntingNightgaunt
      deckCount <- getActDecksInPlayCount
      alejandroVela <- getSetAsideCard Assets.alejandroVela
      leadChooseOrRunOneM do
        targets farthestHuntingNightGaunts \huntingNightgaunt -> do
          createAssetAt_ alejandroVela (AttachedToEnemy huntingNightgaunt)
          when (deckCount <= 2) $ placeDoom attrs huntingNightgaunt 1
      pure a
    FoundEncounterCard _ target card | isTarget attrs target -> do
      locations <- select $ FarthestLocationFromAll Anywhere
      leadChooseOrRunOneM $ targets locations (spawnEnemyAt_ card)
      pure a
    _ -> AtTheStationInShadowedTalons <$> liftRunMessage msg attrs
