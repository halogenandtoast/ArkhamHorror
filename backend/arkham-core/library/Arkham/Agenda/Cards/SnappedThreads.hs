module Arkham.Agenda.Cards.SnappedThreads
  ( SnappedThreads(..)
  , snappedThreads
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype SnappedThreads = SnappedThreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snappedThreads :: AgendaCard SnappedThreads
snappedThreads = agenda (3, A) SnappedThreads Cards.snappedThreads (Static 12)

instance RunMessage SnappedThreads where
  runMessage msg a@(SnappedThreads attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> SnappedThreads <$> runMessage msg attrs
