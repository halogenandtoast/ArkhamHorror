{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.LookWhatIFound where

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

newtype LookWhatIFound = LookWhatIFound Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lookWhatIFound :: InvestigatorId -> EventId -> LookWhatIFound
lookWhatIFound iid uuid = LookWhatIFound $ baseAttrs iid uuid "01079"

instance HasActions env investigator LookWhatIFound where
  getActions i window (LookWhatIFound attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env LookWhatIFound where
  runMessage msg (LookWhatIFound attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      unshiftMessages
        [ InvestigatorDiscoverCluesAtTheirLocation iid 2
        , Discard (EventTarget eid)
        ]
      LookWhatIFound <$> runMessage msg (attrs & resolved .~ True)
    _ -> LookWhatIFound <$> runMessage msg attrs
