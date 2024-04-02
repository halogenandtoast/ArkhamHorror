module Arkham.Event.Cards.BreakingAndEntering2 (breakingAndEntering2, BreakingAndEntering2 (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Prelude

newtype BreakingAndEntering2 = BreakingAndEntering2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingAndEntering2 :: EventCard BreakingAndEntering2
breakingAndEntering2 = event BreakingAndEntering2 Cards.breakingAndEntering2

instance RunMessage BreakingAndEntering2 where
  runMessage msg e@(BreakingAndEntering2 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigation <- mkInvestigate iid attrs
      pushAll [skillTestModifier attrs iid (AddSkillValue #agility), toMessage investigation]
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      when (n >= 1) do
        enemies <- select $ enemyAtLocationWith iid <> EnemyWithoutModifier CannotBeEvaded
        player <- getPlayer iid
        pushIfAny enemies $ chooseOne player $ targetLabels enemies (only . EnemyEvaded iid)
      when (n >= 3) do
        push $ skillTestModifier attrs attrs ReturnToHandAfterTest
      pure e
    _ -> BreakingAndEntering2 <$> runMessage msg attrs
