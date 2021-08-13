module Arkham.Types.Skill.Cards.ViciousBlow2
  ( viciousBlow2
  , ViciousBlow2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype ViciousBlow2 = ViciousBlow2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousBlow2 :: SkillCard ViciousBlow2
viciousBlow2 = skill ViciousBlow2 Cards.viciousBlow2

instance (SkillRunner env) => RunMessage env ViciousBlow2 where
  runMessage msg s@(ViciousBlow2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Fight) _ (SkillTarget sid) _ n | sid == skillId ->
      s <$ push
        (skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (DamageDealt $ if n >= 2 then 2 else 1)
        )
    _ -> ViciousBlow2 <$> runMessage msg attrs
