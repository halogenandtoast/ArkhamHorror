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
import Arkham.SkillType

newtype Leadership = Leadership SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadership :: SkillCard Leadership
leadership = skill Leadership Cards.leadership

instance HasModifiersFor Leadership where
  getModifiersFor (CardIdTarget cid) (Leadership attrs)
    | toCardId attrs == cid = do
        mSkillTestSource <- getSkillTestSource
        case mSkillTestSource of
          Just (SkillTestSource iid' _ _ _)
            | skillOwner attrs /= iid' ->
                pure $ toModifiers attrs [AddSkillIcons [SkillIcon SkillWillpower, WildIcon]]
          _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Leadership where
  runMessage msg (Leadership attrs) = Leadership <$> runMessage msg attrs
