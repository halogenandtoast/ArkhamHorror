module Arkham.Skill.Cards.UnexpectedCourage2 (unexpectedCourage2) where

import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype UnexpectedCourage2 = UnexpectedCourage2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedCourage2 :: SkillCard UnexpectedCourage2
unexpectedCourage2 = skill UnexpectedCourage2 Cards.unexpectedCourage2

instance RunMessage UnexpectedCourage2 where
  runMessage msg s@(UnexpectedCourage2 attrs) = runQueueT $ case msg of
    FailedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      returnToHand iid attrs
      pure s
    _ -> UnexpectedCourage2 <$> liftRunMessage msg attrs
