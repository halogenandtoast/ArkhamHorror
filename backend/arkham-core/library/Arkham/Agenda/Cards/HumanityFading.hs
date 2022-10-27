module Arkham.Agenda.Cards.HumanityFading
  ( HumanityFading(..)
  , humanityFading
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype HumanityFading = HumanityFading AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

humanityFading :: AgendaCard HumanityFading
humanityFading = agenda (2, A) HumanityFading Cards.humanityFading (Static 7)

instance RunMessage HumanityFading where
  runMessage msg a@(HumanityFading attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> HumanityFading <$> runMessage msg attrs
