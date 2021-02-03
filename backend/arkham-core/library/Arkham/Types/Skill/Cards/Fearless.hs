module Arkham.Types.Skill.Cards.Fearless where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Target

newtype Fearless = Fearless SkillAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

fearless :: InvestigatorId -> SkillId -> Fearless
fearless iid uuid = Fearless $ baseAttrs iid uuid "01067"

instance HasModifiersFor env Fearless where
  getModifiersFor = noModifiersFor

instance HasActions env Fearless where
  getActions i window (Fearless attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Fearless where
  runMessage msg s@(Fearless attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ unshiftMessage (HealHorror (InvestigatorTarget skillOwner) 1)
    _ -> Fearless <$> runMessage msg attrs
