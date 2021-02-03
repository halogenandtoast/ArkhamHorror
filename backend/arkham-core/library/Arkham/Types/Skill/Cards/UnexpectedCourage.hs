module Arkham.Types.Skill.Cards.UnexpectedCourage where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId

newtype UnexpectedCourage = UnexpectedCourage SkillAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

unexpectedCourage :: InvestigatorId -> SkillId -> UnexpectedCourage
unexpectedCourage iid uuid = UnexpectedCourage $ baseAttrs iid uuid "01093"

instance HasModifiersFor env UnexpectedCourage where
  getModifiersFor = noModifiersFor

instance HasActions env UnexpectedCourage where
  getActions i window (UnexpectedCourage attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env UnexpectedCourage where
  runMessage msg (UnexpectedCourage attrs) =
    UnexpectedCourage <$> runMessage msg attrs
