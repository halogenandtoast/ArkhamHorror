{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Cards.ViciousBlow where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Source
import Arkham.Types.Target

newtype ViciousBlow = ViciousBlow Attrs
  deriving newtype (Show, ToJSON, FromJSON)

viciousBlow :: InvestigatorId -> SkillId -> ViciousBlow
viciousBlow iid uuid = ViciousBlow $ baseAttrs iid uuid "01025"

instance HasActions env investigator ViciousBlow where
  getActions i window (ViciousBlow attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env ViciousBlow where
  runMessage msg s@(ViciousBlow attrs@Attrs {..}) = case msg of
    SkillTestDidPassBy _ (SkillTarget sid) _ | sid == skillId ->
      s <$ unshiftMessage
        (AddModifier SkillTestTarget (DamageDealt 1 (SkillSource skillId)))
    _ -> ViciousBlow <$> runMessage msg attrs
