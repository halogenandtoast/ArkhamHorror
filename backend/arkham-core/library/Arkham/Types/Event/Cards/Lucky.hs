{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Lucky where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Lucky = Lucky Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lucky :: InvestigatorId -> EventId -> Lucky
lucky iid uuid = Lucky $ baseAttrs iid uuid "01080"

instance HasModifiersFor env Lucky where
  getModifiersFor _ _ _ = pure []

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
