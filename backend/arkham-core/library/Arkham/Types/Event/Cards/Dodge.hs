{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Dodge where

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

newtype Dodge = Dodge Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dodge :: InvestigatorId -> EventId -> Dodge
dodge iid uuid = Dodge $ baseAttrs iid uuid "01023"

instance HasActions env investigator Dodge where
  getActions i window (Dodge attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Dodge where
  runMessage msg (Dodge attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      unshiftMessages [CancelNext AttackMessage, Discard (EventTarget eid)]
      Dodge <$> runMessage msg (attrs & resolved .~ True)
    _ -> Dodge <$> runMessage msg attrs
