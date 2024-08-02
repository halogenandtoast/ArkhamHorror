module Arkham.Event.Cards.DrainEssence (drainEssence, DrainEssence (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Token

newtype DrainEssence = DrainEssence EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drainEssence :: EventCard DrainEssence
drainEssence = event DrainEssence Cards.drainEssence

instance RunMessage DrainEssence where
  runMessage msg e@(DrainEssence attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid <> EnemyWithFight
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      sid <- getRandom
      parley sid iid attrs eid #willpower $ EnemyMaybeFieldCalculation eid EnemyFight
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> do
          isElite <- eid <=~> EliteEnemy
          moveTokens attrs iid eid Damage (if isElite then 1 else 2)
          checkDefeated iid eid
        _ -> pure ()
      pure e
    _ -> DrainEssence <$> liftRunMessage msg attrs
