module Arkham.Event.Cards.BreakingAndEntering (breakingAndEntering, BreakingAndEntering (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Prelude

newtype BreakingAndEntering = BreakingAndEntering EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingAndEntering :: EventCard BreakingAndEntering
breakingAndEntering = event BreakingAndEntering Cards.breakingAndEntering

instance RunMessage BreakingAndEntering where
  runMessage msg e@(BreakingAndEntering attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigation <- mkInvestigate iid attrs
      pushAll [skillTestModifier attrs iid (AddSkillValue #agility), toMessage investigation]
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyWithoutModifier CannotBeEvaded
      player <- getPlayer iid
      pushIfAny enemies $ chooseOne player $ targetLabels enemies (only . EnemyEvaded iid)
      pure e
    _ -> BreakingAndEntering <$> runMessage msg attrs
