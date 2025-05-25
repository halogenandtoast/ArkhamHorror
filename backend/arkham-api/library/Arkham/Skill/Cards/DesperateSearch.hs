module Arkham.Skill.Cards.DesperateSearch (desperateSearch) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype DesperateSearch = DesperateSearch SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desperateSearch :: SkillCard DesperateSearch
desperateSearch = skill DesperateSearch Cards.desperateSearch

instance RunMessage DesperateSearch where
  runMessage msg (DesperateSearch attrs) = DesperateSearch <$> runMessage msg attrs
