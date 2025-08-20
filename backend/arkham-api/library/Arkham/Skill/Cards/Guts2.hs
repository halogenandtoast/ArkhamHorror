module Arkham.Skill.Cards.Guts2 (guts2) where

import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Guts2 = Guts2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guts2 :: SkillCard Guts2
guts2 = skill Guts2 Cards.guts2

instance RunMessage Guts2 where
  runMessage msg s@(Guts2 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ n | sid == skillId attrs -> do
      let amount = if n >= 2 then 2 else 1
      drawCards attrs.owner attrs amount
      pure s
    _ -> Guts2 <$> liftRunMessage msg attrs
