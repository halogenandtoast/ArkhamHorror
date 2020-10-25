{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.DarkMemory where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype DarkMemory = DarkMemory Attrs
  deriving newtype (Show, ToJSON, FromJSON)

darkMemory :: InvestigatorId -> EventId -> DarkMemory
darkMemory iid uuid = DarkMemory $ weaknessAttrs iid uuid "01013"

instance HasModifiersFor env DarkMemory where
  getModifiersFor _ _ _ = pure []

instance HasActions env DarkMemory where
  getActions i window (DarkMemory attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DarkMemory where
  runMessage msg (DarkMemory attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == eventId -> do
      unshiftMessages
        [ PlaceDoomOnAgenda
        , AdvanceAgendaIfThresholdSatisfied
        , Discard (EventTarget eid)
        ]
      DarkMemory <$> runMessage msg (attrs & resolved .~ True)
    _ -> DarkMemory <$> runMessage msg attrs
