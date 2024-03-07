module Arkham.Agenda.Cards.TheDeadWithNoName
  ( TheDeadWithNoName(..)
  , theDeadWithNoName
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheDeadWithNoName = TheDeadWithNoName AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDeadWithNoName :: AgendaCard TheDeadWithNoName
theDeadWithNoName = agenda (3, A) TheDeadWithNoName Cards.theDeadWithNoName (Static 12)

instance RunMessage TheDeadWithNoName where
  runMessage msg a@(TheDeadWithNoName attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheDeadWithNoName <$> runMessage msg attrs
