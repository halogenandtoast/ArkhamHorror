{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Cards.Deduction where

import ClassyPrelude

import Arkham.Json
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Source
import Arkham.Types.Target

newtype Deduction = Deduction Attrs
  deriving newtype (Show, ToJSON, FromJSON)

deduction :: InvestigatorId -> SkillId -> Deduction
deduction iid uuid = Deduction $ baseAttrs iid uuid "01039"

instance HasActions env investigator Deduction where
  getActions i window (Deduction attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env Deduction where
  runMessage msg s@(Deduction attrs@Attrs {..}) = case msg of
    PassedSkillTest _ (Just Action.Investigate) (LocationSource _) (SkillTarget sid) _
      | sid == skillId
      -> s <$ unshiftMessage
        (AddModifiers SkillTestTarget (SkillSource skillId) [DiscoveredClues 1])
    _ -> Deduction <$> runMessage msg attrs
