module Arkham.Event.Events.Waylay (waylay) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype Waylay = Waylay EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waylay :: EventCard Waylay
waylay = event Waylay Cards.waylay

instance RunMessage Waylay where
  runMessage msg e@(Waylay attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ NonEliteEnemy <> enemyAtLocationWith iid <> ExhaustedEnemy <> EnemyWithEvade
      sid <- getRandom
      chooseTargetM iid enemies \enemy ->
        beginSkillTest sid iid attrs enemy #agility (EnemyMaybeFieldCalculation enemy EnemyEvade)
      pure e
    PassedSkillTest iid _ (isSource attrs -> True) (SkillTestInitiatorTarget (EnemyTarget eid)) _ _ -> do
      defeatEnemy eid iid attrs
      pure e
    _ -> Waylay <$> liftRunMessage msg attrs
