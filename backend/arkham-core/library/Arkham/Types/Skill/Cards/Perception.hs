module Arkham.Types.Skill.Cards.Perception where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Target

newtype Perception = Perception Attrs
  deriving newtype (Show, ToJSON, FromJSON)

perception :: InvestigatorId -> SkillId -> Perception
perception iid uuid = Perception $ baseAttrs iid uuid "01090"

instance HasModifiersFor env Perception where
  getModifiersFor = noModifiersFor

instance HasActions env Perception where
  getActions i window (Perception attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Perception where
  runMessage msg s@(Perception attrs@Attrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ unshiftMessage (DrawCards skillOwner 1 False)
    _ -> Perception <$> runMessage msg attrs
