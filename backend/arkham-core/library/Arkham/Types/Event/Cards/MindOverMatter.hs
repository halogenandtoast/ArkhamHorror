{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.MindOverMatter where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype MindOverMatter = MindOverMatter Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mindOverMatter :: InvestigatorId -> EventId -> MindOverMatter
mindOverMatter iid uuid = MindOverMatter $ baseAttrs iid uuid "01036"

instance HasModifiersFor env MindOverMatter where
  getModifiersFor _ _ _ = pure []

instance HasActions env MindOverMatter where
  getActions i window (MindOverMatter attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env MindOverMatter where
  runMessage msg (MindOverMatter attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages
        [ AddModifiers
          (InvestigatorTarget iid)
          (EventSource eid)
          [ UseSkillInPlaceOf SkillCombat SkillIntellect
          , UseSkillInPlaceOf SkillAgility SkillIntellect
          ]
        , Discard (EventTarget eid)
        ]
      MindOverMatter <$> runMessage msg (attrs & resolved .~ True)
    _ -> MindOverMatter <$> runMessage msg attrs
