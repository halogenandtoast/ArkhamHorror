module Arkham.Agenda.Cards.JourneyThroughTheGates
  ( JourneyThroughTheGates(..)
  , journeyThroughTheGates
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype JourneyThroughTheGates = JourneyThroughTheGates AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

journeyThroughTheGates :: AgendaCard JourneyThroughTheGates
journeyThroughTheGates = agenda (1, A) JourneyThroughTheGates Cards.journeyThroughTheGates (Static 19)

instance RunMessage JourneyThroughTheGates where
  runMessage msg a@(JourneyThroughTheGates attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> JourneyThroughTheGates <$> runMessage msg attrs
