module Arkham.Event.Events.BreakingAndEntering (breakingAndEntering) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (investigate_)
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
      investigate_ sid iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeEvadedBy (toSource attrs)
      chooseOrRunOneM iid do
        targets enemies $ automaticallyEvadeEnemy iid
        labeledI "continue" nothing
      pure e
    _ -> BreakingAndEntering <$> liftRunMessage msg attrs
