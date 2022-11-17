module Arkham.Agenda.Cards.FuryThatShakesTheEarth
  ( FuryThatShakesTheEarth(..)
  , furyThatShakesTheEarth
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype FuryThatShakesTheEarth = FuryThatShakesTheEarth AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

furyThatShakesTheEarth :: AgendaCard FuryThatShakesTheEarth
furyThatShakesTheEarth =
  agenda (5, A) FuryThatShakesTheEarth Cards.furyThatShakesTheEarth (Static 5)

instance RunMessage FuryThatShakesTheEarth where
  runMessage msg a@(FuryThatShakesTheEarth attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> FuryThatShakesTheEarth <$> runMessage msg attrs
