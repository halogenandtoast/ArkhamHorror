module Arkham.Event.Events.Stakeout (stakeout) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Modifier

newtype Stakeout = Stakeout EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stakeout :: EventCard Stakeout
stakeout = event Stakeout Cards.stakeout

instance RunMessage Stakeout where
  runMessage msg e@(Stakeout attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #intellect 2)
      investigate_ sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      healHorrorIfCan iid attrs 1
      pure e
    _ -> Stakeout <$> liftRunMessage msg attrs
