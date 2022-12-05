module Arkham.Agenda.Cards.PendolousThreads
  ( PendolousThreads(..)
  , pendolousThreads
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype PendolousThreads = PendolousThreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pendolousThreads :: AgendaCard PendolousThreads
pendolousThreads =
  agenda (2, A) PendolousThreads Cards.pendolousThreads (Static 7)

instance RunMessage PendolousThreads where
  runMessage msg a@(PendolousThreads attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> PendolousThreads <$> runMessage msg attrs
