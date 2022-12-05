module Arkham.Agenda.Cards.ThreadsOfTime
  ( ThreadsOfTime(..)
  , threadsOfTime
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype ThreadsOfTime = ThreadsOfTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threadsOfTime :: AgendaCard ThreadsOfTime
threadsOfTime = agenda (1, A) ThreadsOfTime Cards.threadsOfTime (Static 6)

instance RunMessage ThreadsOfTime where
  runMessage msg a@(ThreadsOfTime attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> ThreadsOfTime <$> runMessage msg attrs
