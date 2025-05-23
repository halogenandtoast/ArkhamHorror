module Arkham.Skill.Cards.Opportunist (opportunist) where

import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Opportunist = Opportunist SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

opportunist :: SkillCard Opportunist
opportunist = skill Opportunist Cards.opportunist

instance RunMessage Opportunist where
  runMessage msg s@(Opportunist attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 3 -> do
      returnToHand iid attrs
      pure s
    _ -> Opportunist <$> liftRunMessage msg attrs
