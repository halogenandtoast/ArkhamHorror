module Arkham.Types.Event.Cards.DelveTooDeep
  ( delveTooDeep
  , DelveTooDeep(..)
  )
where

import Arkham.Import

import Arkham.Types.Event.Attrs

newtype DelveTooDeep = DelveTooDeep Attrs
  deriving newtype (Show, ToJSON, FromJSON)

delveTooDeep :: InvestigatorId -> EventId -> DelveTooDeep
delveTooDeep iid uuid =
  DelveTooDeep $ baseAttrs iid uuid "02111"

instance HasActions env DelveTooDeep where
  getActions iid window (DelveTooDeep attrs) = getActions iid window attrs

instance HasModifiersFor env DelveTooDeep where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env DelveTooDeep where
  runMessage msg e@(DelveTooDeep attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == eventId -> do
      e <$ unshiftMessages [AllDrawEncounterCard, AddToVictory (EventTarget eid)]
    _ -> DelveTooDeep <$> runMessage msg attrs
