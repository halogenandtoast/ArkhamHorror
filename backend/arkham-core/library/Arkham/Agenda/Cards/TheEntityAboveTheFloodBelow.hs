module Arkham.Agenda.Cards.TheEntityAboveTheFloodBelow
  ( TheEntityAboveTheFloodBelow
  , theEntityAboveTheFloodBelow
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.Target

newtype TheEntityAboveTheFloodBelow = TheEntityAboveTheFloodBelow AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEntityAboveTheFloodBelow :: AgendaCard TheEntityAboveTheFloodBelow
theEntityAboveTheFloodBelow = agenda
  (2, C)
  TheEntityAboveTheFloodBelow
  Cards.theEntityAboveTheFloodBelow
  (Static 6)

instance RunMessage TheEntityAboveTheFloodBelow where
  runMessage msg a@(TheEntityAboveTheFloodBelow attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide D attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> TheEntityAboveTheFloodBelow <$> runMessage msg attrs
