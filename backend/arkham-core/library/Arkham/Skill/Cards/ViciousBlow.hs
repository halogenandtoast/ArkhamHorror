module Arkham.Skill.Cards.ViciousBlow (
  viciousBlow,
  ViciousBlow (..),
) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype ViciousBlow = ViciousBlow SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousBlow :: SkillCard ViciousBlow
viciousBlow = skill ViciousBlow Cards.viciousBlow

instance RunMessage ViciousBlow where
  runMessage msg s@(ViciousBlow attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Fight) _ (SkillTarget sid) _ _
      | sid == skillId ->
          s
            <$ push
              (skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1))
    _ -> ViciousBlow <$> runMessage msg attrs
