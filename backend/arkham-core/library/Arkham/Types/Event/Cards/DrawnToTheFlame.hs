{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.DrawnToTheFlame where

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

newtype DrawnToTheFlame = DrawnToTheFlame Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drawnToTheFlame :: InvestigatorId -> EventId -> DrawnToTheFlame
drawnToTheFlame iid uuid = DrawnToTheFlame $ baseAttrs iid uuid "01064"

instance HasActions env investigator DrawnToTheFlame where
  getActions i window (DrawnToTheFlame attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DrawnToTheFlame where
  runMessage msg (DrawnToTheFlame attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages
        [ InvestigatorDrawEncounterCard iid
        , InvestigatorDiscoverCluesAtTheirLocation iid 2
        , Discard (EventTarget eid)
        ]
      DrawnToTheFlame <$> runMessage msg (attrs & resolved .~ True)
    _ -> DrawnToTheFlame <$> runMessage msg attrs
