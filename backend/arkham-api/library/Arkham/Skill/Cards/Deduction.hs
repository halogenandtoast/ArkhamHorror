module Arkham.Skill.Cards.Deduction where

import Arkham.Action qualified as Action
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Message
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Deduction = Deduction SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction :: SkillCard Deduction
deduction = skill Deduction Cards.deduction

instance RunMessage Deduction where
  runMessage msg s@(Deduction attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Action.Investigate) _ (isTarget attrs -> True) _ _ -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (DiscoveredClues 1)
      pure s
    _ -> Deduction <$> liftRunMessage msg attrs
