{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Dodge where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Dodge = Dodge Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dodge :: InvestigatorId -> EventId -> Dodge
dodge iid uuid = Dodge $ baseAttrs iid uuid "01023"

instance HasModifiersFor env Dodge where
  getModifiersFor _ _ _ = pure []

instance HasActions env Dodge where
  getActions i window (Dodge attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Dodge where
  runMessage msg (Dodge attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == eventId -> do
      unshiftMessages [CancelNext AttackMessage, Discard (EventTarget eid)]
      Dodge <$> runMessage msg (attrs & resolved .~ True)
    _ -> Dodge <$> runMessage msg attrs
