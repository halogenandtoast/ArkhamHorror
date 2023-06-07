module Arkham.Agenda.Cards.TheHierophantV
  ( TheHierophantV(..)
  , theHierophantV
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheHierophantV = TheHierophantV AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHierophantV :: AgendaCard TheHierophantV
theHierophantV = agenda (1, A) TheHierophantV Cards.theHierophantV (Static 8)

instance RunMessage TheHierophantV where
  runMessage msg a@(TheHierophantV attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheHierophantV <$> runMessage msg attrs
