module Arkham.Agenda.Cards.ThreeFates
  ( ThreeFates(..)
  , threeFates
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype ThreeFates = ThreeFates AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threeFates :: AgendaCard ThreeFates
threeFates = agenda (1, A) ThreeFates Cards.threeFates (Static 6)

instance RunMessage ThreeFates where
  runMessage msg a@(ThreeFates attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> ThreeFates <$> runMessage msg attrs
