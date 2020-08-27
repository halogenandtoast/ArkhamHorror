{-# LANGUAGE UndecidableInstances #-}
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

newtype Fearless = Fearless Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fearless :: InvestigatorId -> SkillId -> Fearless
fearless iid uuid = Fearless $ baseAttrs iid uuid "01067"

instance HasActions env investigator Fearless where
  getActions i window (Fearless attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Fearless where
  runMessage msg s@(Fearless attrs@Attrs {..}) = case msg of
    PassedSkillTest{} -> s <$ unshiftMessage
      (AddOnSuccess (HealHorror (InvestigatorTarget skillOwner) 1))
    _ -> Fearless <$> runMessage msg attrs
