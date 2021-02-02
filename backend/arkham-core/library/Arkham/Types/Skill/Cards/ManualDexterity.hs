module Arkham.Types.Skill.Cards.ManualDexterity where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Target

newtype ManualDexterity = ManualDexterity Attrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

manualDexterity :: InvestigatorId -> SkillId -> ManualDexterity
manualDexterity iid uuid = ManualDexterity $ baseAttrs iid uuid "01092"

instance HasModifiersFor env ManualDexterity where
  getModifiersFor = noModifiersFor

instance HasActions env ManualDexterity where
  getActions i window (ManualDexterity attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env ManualDexterity where
  runMessage msg s@(ManualDexterity attrs@Attrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ unshiftMessage (DrawCards skillOwner 1 False)
    _ -> ManualDexterity <$> runMessage msg attrs
