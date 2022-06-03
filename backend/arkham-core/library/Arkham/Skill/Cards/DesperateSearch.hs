module Arkham.Skill.Cards.DesperateSearch
  ( desperateSearch
  , DesperateSearch(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Skill.Attrs
import Arkham.Skill.Runner

newtype DesperateSearch = DesperateSearch SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desperateSearch :: SkillCard DesperateSearch
desperateSearch = skill DesperateSearch Cards.desperateSearch

instance SkillRunner env => RunMessage DesperateSearch where
  runMessage msg (DesperateSearch attrs) =
    DesperateSearch <$> runMessage msg attrs
