module Arkham.Agenda.Cards.TheRedDepths
  ( TheRedDepths(..)
  , theRedDepths
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheRedDepths = TheRedDepths AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedDepths :: AgendaCard TheRedDepths
theRedDepths = agenda (6, A) TheRedDepths Cards.theRedDepths (Static 5)

instance RunMessage TheRedDepths where
  runMessage msg a@(TheRedDepths attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> TheRedDepths <$> runMessage msg attrs
