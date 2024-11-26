module Arkham.Act.Cards.NightAtTheMuseum (NightAtTheMuseum (..), nightAtTheMuseum) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers hiding (advancedWithOther)
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers

newtype NightAtTheMuseum = NightAtTheMuseum ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightAtTheMuseum :: ActCard NightAtTheMuseum
nightAtTheMuseum = act (2, A) NightAtTheMuseum Cards.nightAtTheMuseum Nothing

instance HasAbilities NightAtTheMuseum where
  getAbilities (NightAtTheMuseum x) =
    [mkAbility x 1 $ forced $ enters #after You Cards.exhibitHallRestrictedHall]

instance RunMessage NightAtTheMuseum where
  runMessage msg a@(NightAtTheMuseum attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      getHuntingHorror >>= \case
        Just eid -> do
          enemyMoveTo eid =<< getRestrictedHall
          readyThis eid
        Nothing -> do
          lead <- getLead
          findEncounterCardIn lead attrs Enemies.huntingHorror [#deck, #discard, #void]
      advanceActDeck attrs
      pure a
    FoundEnemyInOutOfPlay VoidZone _ (isTarget attrs -> True) eid -> do
      lid <- getRestrictedHall
      pushAll [EnemySpawnFromOutOfPlay VoidZone Nothing lid eid]
      pure a
    FoundEncounterCard _ (isTarget attrs -> True) ec -> do
      spawnEnemyAt_ ec =<< getRestrictedHall
      pure a
    _ -> NightAtTheMuseum <$> liftRunMessage msg attrs
