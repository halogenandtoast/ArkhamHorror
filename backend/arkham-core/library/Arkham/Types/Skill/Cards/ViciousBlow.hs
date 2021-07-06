module Arkham.Types.Skill.Cards.ViciousBlow where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Source
import Arkham.Types.Target

newtype ViciousBlow = ViciousBlow SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousBlow :: SkillCard ViciousBlow
viciousBlow = skill ViciousBlow Cards.viciousBlow

instance HasModifiersFor env ViciousBlow where
  getModifiersFor = noModifiersFor

instance HasActions env ViciousBlow where
  getActions i window (ViciousBlow attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env ViciousBlow where
  runMessage msg s@(ViciousBlow attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Fight) _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ push
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          (SkillSource skillId)
          (InvestigatorTarget iid)
        )
    _ -> ViciousBlow <$> runMessage msg attrs
