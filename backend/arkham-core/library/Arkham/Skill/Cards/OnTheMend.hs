module Arkham.Skill.Cards.OnTheMend (onTheMend, OnTheMend (..)) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (Discarded)

newtype OnTheMend = OnTheMend SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheMend :: SkillCard OnTheMend
onTheMend = skill OnTheMend Cards.onTheMend

instance RunMessage OnTheMend where
  runMessage msg s@(OnTheMend attrs) = runQueueT $ case msg of
    _ -> OnTheMend <$> liftRunMessage msg attrs
