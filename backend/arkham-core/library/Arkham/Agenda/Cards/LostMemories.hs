module Arkham.Agenda.Cards.LostMemories
  ( LostMemories(..)
  , lostMemories
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype LostMemories = LostMemories AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: AgendaCard LostMemories
lostMemories = agenda (2, A) LostMemories Cards.lostMemories (Static 7)

instance RunMessage LostMemories where
  runMessage msg a@(LostMemories attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> LostMemories <$> runMessage msg attrs
