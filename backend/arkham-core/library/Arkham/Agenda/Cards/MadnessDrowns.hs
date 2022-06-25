module Arkham.Agenda.Cards.MadnessDrowns
  ( MadnessDrowns
  , madnessDrowns
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype MadnessDrowns = MadnessDrowns AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessDrowns :: AgendaCard MadnessDrowns
madnessDrowns = agenda (2, A) MadnessDrowns Cards.madnessDrowns (Static 7)

instance RunMessage MadnessDrowns where
  runMessage msg a@(MadnessDrowns attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> MadnessDrowns <$> runMessage msg attrs
