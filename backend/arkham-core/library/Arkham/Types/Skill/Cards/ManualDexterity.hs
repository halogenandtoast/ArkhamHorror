{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Cards.ManualDexterity where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId

newtype ManualDexterity = ManualDexterity Attrs
  deriving newtype (Show, ToJSON, FromJSON)

manualDexterity :: InvestigatorId -> SkillId -> ManualDexterity
manualDexterity iid uuid = ManualDexterity $ baseAttrs iid uuid "01092"

instance HasActions env investigator ManualDexterity where
  getActions i window (ManualDexterity attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env ManualDexterity where
  runMessage msg s@(ManualDexterity attrs@Attrs {..}) = case msg of
    PassedSkillTest{} ->
      s <$ unshiftMessage (AddOnSuccess (DrawCards skillOwner 1 False))
    _ -> ManualDexterity <$> runMessage msg attrs
