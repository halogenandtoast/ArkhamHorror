module Arkham.Agenda.Cards.TheHermitIX
  ( TheHermitIX(..)
  , theHermitIX
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheHermitIX = TheHermitIX AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHermitIX :: AgendaCard TheHermitIX
theHermitIX = agenda (1, A) TheHermitIX Cards.theHermitIX (Static 4)

instance RunMessage TheHermitIX where
  runMessage msg a@(TheHermitIX attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> TheHermitIX <$> runMessage msg attrs
