module Arkham.Skill.Cards.Deduction2 (deduction2) where

import Arkham.Helpers.SkillTest (withSkillTest, isInvestigation)
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Deduction2 = Deduction2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction2 :: SkillCard Deduction2
deduction2 = skill Deduction2 Cards.deduction2

instance RunMessage Deduction2 where
  runMessage msg s@(Deduction2 attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n -> do
      whenM isInvestigation do
        withSkillTest \sid -> skillTestModifier sid attrs iid $ DiscoveredClues $ if n >= 2 then 2 else 1
      pure s
    _ -> Deduction2 <$> liftRunMessage msg attrs
