{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Cards.UnexpectedCourage where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId

newtype UnexpectedCourage = UnexpectedCourage Attrs
  deriving newtype (Show, ToJSON, FromJSON)

unexpectedCourage :: InvestigatorId -> SkillId -> UnexpectedCourage
unexpectedCourage iid uuid = UnexpectedCourage $ baseAttrs iid uuid "01093"

instance HasActions env investigator UnexpectedCourage where
  getActions i window (UnexpectedCourage attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env UnexpectedCourage where
  runMessage msg (UnexpectedCourage attrs) =
    UnexpectedCourage <$> runMessage msg attrs
