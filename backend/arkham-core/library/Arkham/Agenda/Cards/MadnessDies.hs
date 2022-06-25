module Arkham.Agenda.Cards.MadnessDies
  ( MadnessDies
  , madnessDies
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype MadnessDies = MadnessDies AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessDies :: AgendaCard MadnessDies
madnessDies = agenda (3, A) MadnessDies Cards.madnessDies (Static 9)

instance RunMessage MadnessDies where
  runMessage msg a@(MadnessDies attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> MadnessDies <$> runMessage msg attrs
