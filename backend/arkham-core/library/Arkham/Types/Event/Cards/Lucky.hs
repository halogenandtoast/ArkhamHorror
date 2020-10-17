{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Lucky where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype Lucky = Lucky Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lucky :: InvestigatorId -> EventId -> Lucky
lucky iid uuid = Lucky $ baseAttrs iid uuid "01080"

instance HasActions env Lucky where
  getActions i window (Lucky attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Lucky where
  runMessage msg (Lucky attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == eventId -> do
      unshiftMessages
        [ Discard (EventTarget eid)
        , AddModifiers AfterSkillTestTarget (EventSource eid) [AnySkillValue 2]
        ]
      Lucky <$> runMessage msg (attrs & resolved .~ True)
    _ -> Lucky <$> runMessage msg attrs
