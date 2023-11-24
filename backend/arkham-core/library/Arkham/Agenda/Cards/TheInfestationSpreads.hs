module Arkham.Agenda.Cards.TheInfestationSpreads (
  TheInfestationSpreads (..),
  theInfestationSpreads,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheInfestationSpreads = TheInfestationSpreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInfestationSpreads :: AgendaCard TheInfestationSpreads
theInfestationSpreads = agenda (2, A) TheInfestationSpreads Cards.theInfestationSpreads (Static 6)

instance RunMessage TheInfestationSpreads where
  runMessage msg a@(TheInfestationSpreads attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheInfestationSpreads <$> runMessage msg attrs
