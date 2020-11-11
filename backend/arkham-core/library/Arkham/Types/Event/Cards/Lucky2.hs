{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Lucky2 where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Lucky2 = Lucky2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lucky2 :: InvestigatorId -> EventId -> Lucky2
lucky2 iid uuid = Lucky2 $ baseAttrs iid uuid "01084"

instance HasModifiersFor env Lucky2 where
  getModifiersFor = noModifiersFor

instance HasActions env Lucky2 where
  getActions i window (Lucky2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Lucky2 where
  runMessage msg (Lucky2 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages
        [ Discard (EventTarget eid)
        , DrawCards iid 1 False
        , AddModifiers AfterSkillTestTarget (EventSource eid) [AnySkillValue 2]
        ]
      Lucky2 <$> runMessage msg (attrs & resolved .~ True)
    _ -> Lucky2 <$> runMessage msg attrs
