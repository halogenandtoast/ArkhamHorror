module Arkham.Event.Events.BreakingAndEntering2 (breakingAndEntering2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (investigate_)
import Arkham.Matcher
import Arkham.Modifier

newtype BreakingAndEntering2 = BreakingAndEntering2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingAndEntering2 :: EventCard BreakingAndEntering2
breakingAndEntering2 = event BreakingAndEntering2 Cards.breakingAndEntering2

instance RunMessage BreakingAndEntering2 where
  runMessage msg e@(BreakingAndEntering2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #agility)
      investigate_ sid iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      when (n >= 1) $ skillTestCardOption attrs $ doStep 1 msg
      when (n >= 3) $ atEndOfTurn attrs iid $ addToHand iid (only attrs)
      pure e
    -- See CleanSweep2: the enemies are gathered when the option resolves, so the
    -- investigation's own clue discovery has already had its say.
    DoStep 1 (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      chooseAutomaticallyEvadeAt iid attrs (locationWithInvestigator iid) AnyEnemy
      pure e
    _ -> BreakingAndEntering2 <$> liftRunMessage msg attrs
