module Arkham.Skill.Cards.ViciousBlow2 (viciousBlow2, ViciousBlow2 (..)) where

import Arkham.Action
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype ViciousBlow2 = ViciousBlow2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousBlow2 :: SkillCard ViciousBlow2
viciousBlow2 = skill ViciousBlow2 Cards.viciousBlow2

instance RunMessage ViciousBlow2 where
  runMessage msg s@(ViciousBlow2 attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Fight) _ (SkillTarget sid) _ n | sid == attrs.id -> do
      withSkillTest \stId -> skillTestModifier stId attrs iid (DamageDealt $ if n >= 2 then 2 else 1)
      pure s
    _ -> ViciousBlow2 <$> liftRunMessage msg attrs
