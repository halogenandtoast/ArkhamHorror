module Arkham.Skill.Cards.Leadership (leadership, Leadership (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Leadership = Leadership SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadership :: SkillCard Leadership
leadership = skill Leadership Cards.leadership

instance HasModifiersFor Leadership where
  getModifiersFor (Leadership attrs) = do
    mInvestigator <- getSkillTestInvestigator
    case mInvestigator of
      Just iid | skillOwner attrs /= iid -> do
        modified_ attrs (CardIdTarget $ toCardId attrs) [AddSkillIcons [#willpower, #wild]]
      _ -> pure mempty

instance RunMessage Leadership where
  runMessage msg (Leadership attrs) = Leadership <$> runMessage msg attrs
