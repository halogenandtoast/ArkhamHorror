{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.EmergencyCache where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype EmergencyCache = EmergencyCache Attrs
  deriving newtype (Show, ToJSON, FromJSON)

emergencyCache :: InvestigatorId -> EventId -> EmergencyCache
emergencyCache iid uuid = EmergencyCache $ baseAttrs iid uuid "01088"

instance HasActions env investigator EmergencyCache where
  getActions i window (EmergencyCache attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env EmergencyCache where
  runMessage msg (EmergencyCache attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      unshiftMessages [TakeResources iid 3 False, Discard (EventTarget eid)]
      EmergencyCache <$> runMessage msg (attrs & resolved .~ True)
    _ -> EmergencyCache <$> runMessage msg attrs
