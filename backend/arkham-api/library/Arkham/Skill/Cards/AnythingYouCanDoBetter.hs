module Arkham.Skill.Cards.AnythingYouCanDoBetter (anythingYouCanDoBetter) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype AnythingYouCanDoBetter = AnythingYouCanDoBetter SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anythingYouCanDoBetter :: SkillCard AnythingYouCanDoBetter
anythingYouCanDoBetter = skill AnythingYouCanDoBetter Cards.anythingYouCanDoBetter

instance RunMessage AnythingYouCanDoBetter where
  runMessage msg (AnythingYouCanDoBetter attrs) = AnythingYouCanDoBetter <$> runMessage msg attrs
