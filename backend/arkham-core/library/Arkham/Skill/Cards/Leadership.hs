module Arkham.Skill.Cards.Leadership (
  leadership,
  Leadership (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Leadership = Leadership SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadership :: SkillCard Leadership
leadership = skill Leadership Cards.leadership

instance HasModifiersFor Leadership where
  getModifiersFor (CardIdTarget cid) (Leadership attrs) | toCardId attrs == cid = do
    mInvestigator <- getSkillTestInvestigator
    pure $ case mInvestigator of
      Just iid | skillOwner attrs /= iid -> do
        toModifiers attrs [AddSkillIcons [#willpower, #wild]]
      _ -> []
  getModifiersFor _ _ = pure []

instance RunMessage Leadership where
  runMessage msg (Leadership attrs) = Leadership <$> runMessage msg attrs
