module Arkham.Skill.Cards.Inquisitive1 (inquisitive1) where

import Arkham.I18n
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
          withI18n $ countVar 2 $ labeledI "passByMore" (go 2)
          withI18n $ countVar 1 $ labeledI "passByMore" (go 1)
          labeledI "doNotModify" nothing
          when (n > 0) $ withI18n $ countVar 1 $ labeledI "passByLess" (go (-1))
          when (n > 1) $ withI18n $ countVar 2 $ labeledI "passByLess" (go (-2))
      pure s
    _ -> Inquisitive1 <$> liftRunMessage msg attrs
