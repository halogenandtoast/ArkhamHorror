module Arkham.Treachery.Cards.ReclaimedByNature (reclaimedByNature) where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ReclaimedByNature = ReclaimedByNature TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reclaimedByNature :: TreacheryCard ReclaimedByNature
reclaimedByNature = treachery ReclaimedByNature Cards.reclaimedByNature

instance RunMessage ReclaimedByNature where
  runMessage msg t@(ReclaimedByNature attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      getTimeFor iid >>= \case
        Day -> revelationSkillTest sid iid attrs #combat (Fixed 3)
        Night -> revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      getTimeFor iid >>= \case
        Day -> do
          nearestEnemies <- select $ NearestEnemyTo iid AnyEnemy
          chooseOneM iid $ campaignI18n do
            unscoped $ countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
            when (notNull nearestEnemies) do
              labeled' "reclaimedByNature.nearestEnemyAttacks" do
                chooseTargetM iid nearestEnemies \enemy ->
                  initiateEnemyAttackEdit enemy attrs iid despiteExhausted
        Night -> do
          let yourLocation = locationWithInvestigator iid
          enemies <- select $ EnemyWithAnyDamage <> EnemyAt (oneOf [yourLocation, connectedTo yourLocation])
          chooseOneM iid $ campaignI18n do
            unscoped $ countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
            when (notNull enemies) do
              labeled' "reclaimedByNature.enemiesHeal" do
                for_ enemies \enemy -> healDamage enemy attrs 1
      pure t
    _ -> ReclaimedByNature <$> liftRunMessage msg attrs
