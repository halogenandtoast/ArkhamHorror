module Arkham.Skill.Cards.Perception2 (perception2) where

import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Perception2 = Perception2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perception2 :: SkillCard Perception2
perception2 = skill Perception2 Cards.perception2

instance RunMessage Perception2 where
  runMessage msg s@(Perception2 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ n | sid == skillId attrs -> do
      let amount = if n >= 2 then 2 else 1
      skillTestResultOption "Perception (2)" do
        drawCards attrs.owner attrs amount
      pure s
    _ -> Perception2 <$> liftRunMessage msg attrs
