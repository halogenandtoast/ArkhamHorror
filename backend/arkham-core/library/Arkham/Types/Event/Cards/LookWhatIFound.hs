{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.LookWhatIFound where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype LookWhatIFound = LookWhatIFound Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lookWhatIFound :: InvestigatorId -> EventId -> LookWhatIFound
lookWhatIFound iid uuid = LookWhatIFound $ baseAttrs iid uuid "01079"

instance HasModifiersFor env LookWhatIFound where
  getModifiersFor = noModifiersFor

instance HasActions env LookWhatIFound where
  getActions i window (LookWhatIFound attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env LookWhatIFound where
  runMessage msg e@(LookWhatIFound attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      e <$ unshiftMessages
        [ InvestigatorDiscoverCluesAtTheirLocation iid 2
        , Discard (EventTarget eid)
        ]
    _ -> LookWhatIFound <$> runMessage msg attrs
