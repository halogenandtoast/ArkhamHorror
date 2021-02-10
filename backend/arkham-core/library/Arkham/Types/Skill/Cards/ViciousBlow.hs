module Arkham.Types.Skill.Cards.ViciousBlow where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillId
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Action
import Arkham.Types.Game.Helpers
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype ViciousBlow = ViciousBlow SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousBlow :: InvestigatorId -> SkillId -> ViciousBlow
viciousBlow iid uuid = ViciousBlow $ baseAttrs iid uuid "01025"

instance HasModifiersFor env ViciousBlow where
  getModifiersFor = noModifiersFor

instance HasActions env ViciousBlow where
  getActions i window (ViciousBlow attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env ViciousBlow where
  runMessage msg s@(ViciousBlow attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Fight) _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          (SkillSource skillId)
          (InvestigatorTarget iid)
        )
    _ -> ViciousBlow <$> runMessage msg attrs
