module Arkham.Event.Cards.BreakingAndEntering (breakingAndEntering, BreakingAndEntering (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigate
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier

newtype BreakingAndEntering = BreakingAndEntering EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingAndEntering :: EventCard BreakingAndEntering
breakingAndEntering = event BreakingAndEntering Cards.breakingAndEntering

instance RunMessage BreakingAndEntering where
  runMessage msg e@(BreakingAndEntering attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #agility)
      pushM $ mkInvestigate sid iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyWithoutModifier CannotBeEvaded
      when (notNull enemies) $ chooseOne iid $ targetLabels enemies (only . EnemyEvaded iid)
      pure e
    _ -> BreakingAndEntering <$> liftRunMessage msg attrs
