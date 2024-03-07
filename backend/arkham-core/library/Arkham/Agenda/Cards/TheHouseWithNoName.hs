module Arkham.Agenda.Cards.TheHouseWithNoName
  ( TheHouseWithNoName(..)
  , theHouseWithNoName
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheHouseWithNoName = TheHouseWithNoName AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseWithNoName :: AgendaCard TheHouseWithNoName
theHouseWithNoName = agenda (1, A) TheHouseWithNoName Cards.theHouseWithNoName (Static 12)

instance RunMessage TheHouseWithNoName where
  runMessage msg a@(TheHouseWithNoName attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheHouseWithNoName <$> runMessage msg attrs
