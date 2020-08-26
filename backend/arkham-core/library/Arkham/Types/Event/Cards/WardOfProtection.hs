{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.WardOfProtection where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype WardOfProtection = WardOfProtection Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wardOfProtection :: InvestigatorId -> EventId -> WardOfProtection
wardOfProtection iid uuid = WardOfProtection $ baseAttrs iid uuid "01065"

instance HasActions env investigator WardOfProtection where
  getActions i window (WardOfProtection attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env WardOfProtection where
  runMessage msg (WardOfProtection attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      unshiftMessages
        [ CancelNext RevelationMessage
        , InvestigatorAssignDamage iid (EventSource eid) 0 1
        , Discard (EventTarget eid)
        ]
      WardOfProtection <$> runMessage msg (attrs & resolved .~ True)
    _ -> WardOfProtection <$> runMessage msg attrs
