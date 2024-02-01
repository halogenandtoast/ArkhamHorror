module Arkham.Skill.Cards.ViciousBlow2 (
  viciousBlow2,
  ViciousBlow2 (..),
) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype ViciousBlow2 = ViciousBlow2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

viciousBlow2 :: SkillCard ViciousBlow2
viciousBlow2 = skill ViciousBlow2 Cards.viciousBlow2

instance RunMessage ViciousBlow2 where
  runMessage msg s@(ViciousBlow2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Fight) _ (SkillTarget sid) _ n
      | sid == skillId ->
          s
            <$ push
              ( skillTestModifier
                  attrs
                  (InvestigatorTarget iid)
                  (DamageDealt $ if n >= 2 then 2 else 1)
              )
    _ -> ViciousBlow2 <$> runMessage msg attrs
