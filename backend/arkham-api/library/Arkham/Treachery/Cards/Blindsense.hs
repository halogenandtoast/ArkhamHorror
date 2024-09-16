module Arkham.Treachery.Cards.Blindsense (blindsense, Blindsense (..)) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Zone

newtype Blindsense = Blindsense TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindsense :: TreacheryCard Blindsense
blindsense = treachery Blindsense Cards.blindsense

instance RunMessage Blindsense where
  runMessage msg t@(Blindsense attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      selectOne (OutOfPlayEnemy TheDepths $ enemyIs Enemies.theAmalgam) >>= \case
        Just theAmalgam -> do
          withLocationOf iid \lid -> do
            push $ EnemySpawnFromOutOfPlay TheDepths (Just iid) lid theAmalgam
            initiateEnemyAttack theAmalgam attrs iid
        Nothing ->
          selectOne (enemyIs Enemies.theAmalgam) >>= traverse_ \theAmalgam -> do
            ready theAmalgam
            enemyEngageInvestigator theAmalgam iid
            initiateEnemyAttack theAmalgam attrs iid
      pure t
    _ -> Blindsense <$> liftRunMessage msg attrs
