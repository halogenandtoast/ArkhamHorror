{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.DarkMemory where

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

newtype DarkMemory = DarkMemory Attrs
  deriving newtype (Show, ToJSON, FromJSON)

darkMemory :: InvestigatorId -> EventId -> DarkMemory
darkMemory iid uuid = DarkMemory $ baseAttrs iid uuid "01013"

instance HasActions env investigator DarkMemory where
  getActions i window (DarkMemory attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DarkMemory where
  runMessage msg (DarkMemory attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent _ eid | eid == eventId -> do
      unshiftMessages
        [ PlaceDoomOnAgenda
        , AdvanceAgendaIfThresholdSatisfied
        , Discard (EventTarget eid)
        ]
      DarkMemory <$> runMessage msg (attrs & resolved .~ True)
    _ -> DarkMemory <$> runMessage msg attrs
