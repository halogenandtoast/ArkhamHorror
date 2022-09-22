module Arkham.Agenda.Cards.TheBoundaryBroken
  ( TheBoundaryBroken(..)
  , theBoundaryBroken
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheBoundaryBroken = TheBoundaryBroken AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBoundaryBroken :: AgendaCard TheBoundaryBroken
theBoundaryBroken =
  agenda (1, A) TheBoundaryBroken Cards.theBoundaryBroken (Static 5)

instance RunMessage TheBoundaryBroken where
  runMessage msg a@(TheBoundaryBroken attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> TheBoundaryBroken <$> runMessage msg attrs
