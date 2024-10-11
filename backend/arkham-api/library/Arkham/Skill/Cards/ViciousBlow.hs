module Arkham.Skill.Cards.ViciousBlow (viciousBlow, ViciousBlow (..)) where

import Arkham.Action
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype ViciousBlow = ViciousBlow SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousBlow :: SkillCard ViciousBlow
viciousBlow = skill ViciousBlow Cards.viciousBlow

instance RunMessage ViciousBlow where
  runMessage msg s@(ViciousBlow attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Fight) _ (isTarget attrs -> True) _ _ -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (DamageDealt 1)
      pure s
    _ -> ViciousBlow <$> liftRunMessage msg attrs
