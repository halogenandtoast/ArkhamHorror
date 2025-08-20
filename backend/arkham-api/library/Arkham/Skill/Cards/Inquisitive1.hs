module Arkham.Skill.Cards.Inquisitive1 (inquisitive1) where

import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Inquisitive1 = Inquisitive1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inquisitive1 :: SkillCard Inquisitive1
inquisitive1 = skill Inquisitive1 Cards.inquisitive1

instance RunMessage Inquisitive1 where
  runMessage msg s@(Inquisitive1 attrs) = runQueueT $ case msg of
    Will (PassedSkillTest iid _ _ (isTarget attrs -> True) _ n) -> do
      onlyOnceDuringSkillTest attrs \sid -> do
        let
          go x = do
            skillTestModifier sid attrs sid (SkillTestResultValueModifier x)
            push RecalculateSkillTestResults
        chooseOneM iid do
          labeled "Pass by 2 more" (go 2)
          labeled "Pass by 1 more" (go 1)
          labeled "Do not modify" nothing
          when (n > 0) $ labeled "Pass by 1 less" (go (-1))
          when (n > 1) $ labeled "Pass by 2 less" (go (-2))
      pure s
    _ -> Inquisitive1 <$> liftRunMessage msg attrs
