module Arkham.Agenda.Cards.TheAlarmIsRaised
  ( TheAlarmIsRaised(..)
  , theAlarmIsRaised
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheAlarmIsRaised = TheAlarmIsRaised AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAlarmIsRaised :: AgendaCard TheAlarmIsRaised
theAlarmIsRaised = agenda (2, A) TheAlarmIsRaised Cards.theAlarmIsRaised (Static 12)

instance RunMessage TheAlarmIsRaised where
  runMessage msg a@(TheAlarmIsRaised attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheAlarmIsRaised <$> runMessage msg attrs
