module Arkham.Skill.Cards.Leadership2 (leadership2) where

import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Leadership2 = Leadership2 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadership2 :: SkillCard Leadership2
leadership2 = skill Leadership2 Cards.leadership2

instance HasModifiersFor Leadership2 where
  getModifiersFor (Leadership2 attrs) = do
    withSkillTestInvestigator \iid ->
      addSkillIconsWhen attrs (attrs.owner /= iid) [#willpower, #wild]

instance RunMessage Leadership2 where
  runMessage msg s@(Leadership2 attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      gainResources attrs.owner attrs 2
      when (iid /= attrs.owner) $ gainResources iid attrs 2
      pure s
    _ -> Leadership2 <$> liftRunMessage msg attrs
