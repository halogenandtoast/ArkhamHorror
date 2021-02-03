module Arkham.Types.Skill.Cards.Overpower where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Target

newtype Overpower = Overpower SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overpower :: InvestigatorId -> SkillId -> Overpower
overpower iid uuid = Overpower $ baseAttrs iid uuid "01091"

instance HasModifiersFor env Overpower where
  getModifiersFor = noModifiersFor

instance HasActions env Overpower where
  getActions i window (Overpower attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Overpower where
  runMessage msg s@(Overpower attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ unshiftMessage (DrawCards skillOwner 1 False)
    _ -> Overpower <$> runMessage msg attrs
