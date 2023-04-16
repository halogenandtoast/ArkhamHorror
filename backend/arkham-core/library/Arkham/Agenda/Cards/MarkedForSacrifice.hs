module Arkham.Agenda.Cards.MarkedForSacrifice
  ( MarkedForSacrifice(..)
  , markedForSacrifice
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype MarkedForSacrifice = MarkedForSacrifice AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedForSacrifice :: AgendaCard MarkedForSacrifice
markedForSacrifice =
  agenda (4, A) MarkedForSacrifice Cards.markedForSacrifice (Static 8)

instance RunMessage MarkedForSacrifice where
  runMessage msg a@(MarkedForSacrifice attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> MarkedForSacrifice <$> runMessage msg attrs
