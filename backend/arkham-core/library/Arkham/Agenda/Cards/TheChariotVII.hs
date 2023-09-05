module Arkham.Agenda.Cards.TheChariotVII
  ( TheChariotVII(..)
  , theChariotVII
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheChariotVII = TheChariotVII AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChariotVII :: AgendaCard TheChariotVII
theChariotVII = agenda (1, A) TheChariotVII Cards.theChariotVII (Static 7)

instance RunMessage TheChariotVII where
  runMessage msg a@(TheChariotVII attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheChariotVII <$> runMessage msg attrs
