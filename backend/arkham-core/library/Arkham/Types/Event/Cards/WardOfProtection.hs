{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.WardOfProtection where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype WardOfProtection = WardOfProtection Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wardOfProtection :: InvestigatorId -> EventId -> WardOfProtection
wardOfProtection iid uuid = WardOfProtection $ baseAttrs iid uuid "01065"

instance HasModifiersFor env WardOfProtection where
  getModifiersFor _ _ _ = pure []

instance HasActions env WardOfProtection where
  getActions i window (WardOfProtection attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env WardOfProtection where
  runMessage msg (WardOfProtection attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages
        [ CancelNext RevelationMessage
        , InvestigatorAssignDamage iid (EventSource eid) 0 1
        , Discard (EventTarget eid)
        ]
      WardOfProtection <$> runMessage msg (attrs & resolved .~ True)
    _ -> WardOfProtection <$> runMessage msg attrs
