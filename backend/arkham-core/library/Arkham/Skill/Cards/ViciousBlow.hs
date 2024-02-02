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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

viciousBlow :: SkillCard ViciousBlow
viciousBlow = skill ViciousBlow Cards.viciousBlow

instance RunMessage ViciousBlow where
  runMessage msg s@(ViciousBlow attrs) = case msg of
    PassedSkillTest iid (Just Fight) _ (isTarget attrs -> True) _ _ -> do
      push $ skillTestModifier attrs iid (DamageDealt 1)
      pure s
    _ -> ViciousBlow <$> runMessage msg attrs
