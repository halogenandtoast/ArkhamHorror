module Arkham.Event.Events.BreakingAndEntering2 (breakingAndEntering2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigate
import Arkham.Matcher hiding (EnemyEvaded)
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
      when (n >= 1) do
        enemies <- select $ enemyAtLocationWith iid <> EnemyWithoutModifier CannotBeEvaded
        chooseTargetM iid enemies $ automaticallyEvadeEnemy iid
      when (n >= 3) do
        atEndOfTurn attrs iid do
          addToHand iid (only attrs)
      pure e
    _ -> BreakingAndEntering2 <$> liftRunMessage msg attrs
