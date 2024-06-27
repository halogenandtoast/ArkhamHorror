module Arkham.Event.Cards.BreakingAndEntering2 (breakingAndEntering2, BreakingAndEntering2 (..)) where

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
      skillTestModifier attrs iid (AddSkillValue #agility)
      pushM $ mkInvestigate iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      when (n >= 1) do
        enemies <- select $ enemyAtLocationWith iid <> EnemyWithoutModifier CannotBeEvaded
        when (notNull enemies) $ chooseOne iid $ targetLabels enemies (only . EnemyEvaded iid)
      when (n >= 3) $ skillTestModifier attrs attrs ReturnToHandAfterTest
      pure e
    _ -> BreakingAndEntering2 <$> lift (runMessage msg attrs)
