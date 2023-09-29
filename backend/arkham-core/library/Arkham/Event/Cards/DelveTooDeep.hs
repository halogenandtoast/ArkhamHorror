module Arkham.Event.Cards.DelveTooDeep (
  delveTooDeep,
  DelveTooDeep (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype DelveTooDeep = DelveTooDeep EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delveTooDeep :: EventCard DelveTooDeep
delveTooDeep = event DelveTooDeep Cards.delveTooDeep

instance RunMessage DelveTooDeep where
  runMessage msg e@(DelveTooDeep attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == eventId -> do
      e <$ pushAll [AllDrawEncounterCard, AddToVictory (toTarget attrs)]
    _ -> DelveTooDeep <$> runMessage msg attrs
