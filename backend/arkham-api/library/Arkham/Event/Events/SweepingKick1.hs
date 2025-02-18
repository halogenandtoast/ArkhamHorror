module Arkham.Event.Events.SweepingKick1 (sweepingKick1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Modifier

newtype SweepingKick1 = SweepingKick1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sweepingKick1 :: EventCard SweepingKick1
sweepingKick1 = event SweepingKick1 Cards.sweepingKick1

instance RunMessage SweepingKick1 where
  runMessage msg e@(SweepingKick1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [AddSkillValue #agility, DamageDealt 1]
      chooseFightEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ (automaticallyEvadeEnemy iid)
      pure e
    _ -> SweepingKick1 <$> liftRunMessage msg attrs
