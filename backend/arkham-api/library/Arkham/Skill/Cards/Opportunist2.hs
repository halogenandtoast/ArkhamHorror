module Arkham.Skill.Cards.Opportunist2 (opportunist2) where

import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Opportunist2 = Opportunist2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

opportunist2 :: SkillCard Opportunist2
opportunist2 = skill Opportunist2 Cards.opportunist2

instance RunMessage Opportunist2 where
  runMessage msg s@(Opportunist2 attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 2 -> do
      returnToHand iid attrs
      pure s
    _ -> Opportunist2 <$> liftRunMessage msg attrs
