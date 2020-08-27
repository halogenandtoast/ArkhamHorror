{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Cards.SurvivalInstinct where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId

newtype SurvivalInstinct = SurvivalInstinct Attrs
  deriving newtype (Show, ToJSON, FromJSON)

survivalInstinct :: InvestigatorId -> SkillId -> SurvivalInstinct
survivalInstinct iid uuid = SurvivalInstinct $ baseAttrs iid uuid "01081"

instance HasActions env investigator SurvivalInstinct where
  getActions i window (SurvivalInstinct attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env SurvivalInstinct where
  runMessage msg s@(SurvivalInstinct attrs@Attrs {..}) = case msg of
    PassedSkillTest _ (Just Evade) _ _ -> pure s -- TODO: fill this in
    _ -> SurvivalInstinct <$> runMessage msg attrs
