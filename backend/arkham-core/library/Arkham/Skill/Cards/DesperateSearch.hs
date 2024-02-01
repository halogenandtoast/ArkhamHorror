module Arkham.Skill.Cards.DesperateSearch (
  desperateSearch,
  DesperateSearch (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype DesperateSearch = DesperateSearch SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

desperateSearch :: SkillCard DesperateSearch
desperateSearch = skill DesperateSearch Cards.desperateSearch

instance RunMessage DesperateSearch where
  runMessage msg (DesperateSearch attrs) =
    DesperateSearch <$> runMessage msg attrs
