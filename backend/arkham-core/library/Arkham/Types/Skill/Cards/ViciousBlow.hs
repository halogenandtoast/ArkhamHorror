module Arkham.Types.Skill.Cards.ViciousBlow
  ( viciousBlow
  , ViciousBlow(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Skill.Attrs
import Arkham.Types.Target

newtype ViciousBlow = ViciousBlow SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousBlow :: SkillCard ViciousBlow
viciousBlow = skill ViciousBlow Cards.viciousBlow

instance RunMessage env ViciousBlow where
  runMessage msg s@(ViciousBlow attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Fight) _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ push
        (skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1))
    _ -> ViciousBlow <$> runMessage msg attrs
