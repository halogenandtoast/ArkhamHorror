{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.DrawnToTheFlame where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype DrawnToTheFlame = DrawnToTheFlame Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drawnToTheFlame :: InvestigatorId -> EventId -> DrawnToTheFlame
drawnToTheFlame iid uuid = DrawnToTheFlame $ baseAttrs iid uuid "01064"

instance HasModifiersFor env DrawnToTheFlame where
  getModifiersFor = noModifiersFor

instance HasActions env DrawnToTheFlame where
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
