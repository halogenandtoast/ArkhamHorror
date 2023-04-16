module Arkham.Agenda.Cards.TheWitchLight
  ( TheWitchLight(..)
  , theWitchLight
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheWitchLight = TheWitchLight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWitchLight :: AgendaCard TheWitchLight
theWitchLight = agenda (3, A) TheWitchLight Cards.theWitchLight (Static 8)

instance RunMessage TheWitchLight where
  runMessage msg a@(TheWitchLight attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> TheWitchLight <$> runMessage msg attrs
