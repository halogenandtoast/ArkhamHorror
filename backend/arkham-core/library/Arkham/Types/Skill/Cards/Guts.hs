module Arkham.Types.Skill.Cards.Guts where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Target

newtype Guts = Guts SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guts :: InvestigatorId -> SkillId -> Guts
guts iid uuid = Guts $ baseAttrs iid uuid "01089"

instance HasModifiersFor env Guts where
  getModifiersFor = noModifiersFor

instance HasActions env Guts where
  getActions i window (Guts attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Guts where
  runMessage msg s@(Guts attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ unshiftMessage (DrawCards skillOwner 1 False)
    _ -> Guts <$> runMessage msg attrs
