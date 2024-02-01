module Arkham.Skill.Cards.Deduction where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Deduction = Deduction SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

deduction :: SkillCard Deduction
deduction = skill Deduction Cards.deduction

instance RunMessage Deduction where
  runMessage msg s@(Deduction attrs) = case msg of
    PassedSkillTest iid (Just Action.Investigate) _ (isTarget attrs -> True) _ _ -> do
      push $ skillTestModifier attrs iid (DiscoveredClues 1)
      pure s
    _ -> Deduction <$> runMessage msg attrs
