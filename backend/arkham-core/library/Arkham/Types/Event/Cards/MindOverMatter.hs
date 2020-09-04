{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.MindOverMatter where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype MindOverMatter = MindOverMatter Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mindOverMatter :: InvestigatorId -> EventId -> MindOverMatter
mindOverMatter iid uuid = MindOverMatter $ baseAttrs iid uuid "01036"

instance HasActions env investigator MindOverMatter where
  getActions i window (MindOverMatter attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env MindOverMatter where
  runMessage msg (MindOverMatter attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      unshiftMessages
        [ AddModifier
          (InvestigatorTarget iid)
          (EventSource eid)
          (UseSkillInPlaceOf SkillCombat SkillIntellect)
        , AddModifier
          (InvestigatorTarget iid)
          (EventSource eid)
          (UseSkillInPlaceOf SkillAgility SkillIntellect)
        , Discard (EventTarget eid)
        ]
      MindOverMatter <$> runMessage msg (attrs & resolved .~ True)
    _ -> MindOverMatter <$> runMessage msg attrs
