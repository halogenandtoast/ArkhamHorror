module Arkham.Agenda.Cards.TheThingWithNoName
  ( TheThingWithNoName(..)
  , theThingWithNoName
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheThingWithNoName = TheThingWithNoName AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThingWithNoName :: AgendaCard TheThingWithNoName
theThingWithNoName = agenda (2, A) TheThingWithNoName Cards.theThingWithNoName (Static 12)

instance RunMessage TheThingWithNoName where
  runMessage msg a@(TheThingWithNoName attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheThingWithNoName <$> runMessage msg attrs
