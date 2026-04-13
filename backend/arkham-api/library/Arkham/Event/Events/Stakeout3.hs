module Arkham.Event.Events.Stakeout3 (stakeout3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Modifier

newtype Stakeout3 = Stakeout3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stakeout3 :: EventCard Stakeout3
stakeout3 = event Stakeout3 Cards.stakeout3

instance RunMessage Stakeout3 where
  runMessage msg e@(Stakeout3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #intellect 3, DiscoveredClues 1]
      investigate_ sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      healHorrorIfCan iid attrs 2
      pure e
    _ -> Stakeout3 <$> liftRunMessage msg attrs
