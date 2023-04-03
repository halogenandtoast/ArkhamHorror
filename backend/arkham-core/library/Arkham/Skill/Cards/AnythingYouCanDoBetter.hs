module Arkham.Skill.Cards.AnythingYouCanDoBetter
  ( anythingYouCanDoBetter
  , AnythingYouCanDoBetter(..)
  )
where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Skill.Runner

newtype AnythingYouCanDoBetter = AnythingYouCanDoBetter SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anythingYouCanDoBetter :: SkillCard AnythingYouCanDoBetter
anythingYouCanDoBetter =
  skill AnythingYouCanDoBetter Cards.anythingYouCanDoBetter

instance RunMessage AnythingYouCanDoBetter where
  runMessage msg (AnythingYouCanDoBetter attrs) = AnythingYouCanDoBetter <$> runMessage msg attrs
