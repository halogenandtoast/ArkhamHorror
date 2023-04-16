module Arkham.Agenda.Cards.TheFamiliar
  ( TheFamiliar(..)
  , theFamiliar
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheFamiliar = TheFamiliar AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFamiliar :: AgendaCard TheFamiliar
theFamiliar = agenda (2, A) TheFamiliar Cards.theFamiliar (Static 6)

instance RunMessage TheFamiliar where
  runMessage msg a@(TheFamiliar attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> TheFamiliar <$> runMessage msg attrs
