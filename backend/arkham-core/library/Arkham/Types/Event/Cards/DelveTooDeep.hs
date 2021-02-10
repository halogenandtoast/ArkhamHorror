module Arkham.Types.Event.Cards.DelveTooDeep
  ( delveTooDeep
  , DelveTooDeep(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Event.Attrs

newtype DelveTooDeep = DelveTooDeep EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delveTooDeep :: InvestigatorId -> EventId -> DelveTooDeep
delveTooDeep iid uuid = DelveTooDeep $ baseAttrs iid uuid "02111"

instance HasActions env DelveTooDeep where
  getActions iid window (DelveTooDeep attrs) = getActions iid window attrs

instance HasModifiersFor env DelveTooDeep where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env DelveTooDeep where
  runMessage msg e@(DelveTooDeep attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == eventId -> do
      e <$ unshiftMessages [AllDrawEncounterCard, AddToVictory (toTarget attrs)]
    _ -> DelveTooDeep <$> runMessage msg attrs
