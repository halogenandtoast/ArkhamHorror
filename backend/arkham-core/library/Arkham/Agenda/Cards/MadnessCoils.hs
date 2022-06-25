module Arkham.Agenda.Cards.MadnessCoils
  ( MadnessCoils
  , madnessCoils
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype MadnessCoils = MadnessCoils AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessCoils :: AgendaCard MadnessCoils
madnessCoils = agenda (1, A) MadnessCoils Cards.madnessCoils (Static 7)

instance RunMessage MadnessCoils where
  runMessage msg a@(MadnessCoils attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> MadnessCoils <$> runMessage msg attrs
