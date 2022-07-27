module Arkham.Agenda.Cards.ExpeditionIntoTheWild
  ( ExpeditionIntoTheWild(..)
  , expeditionIntoTheWild
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype ExpeditionIntoTheWild = ExpeditionIntoTheWild AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionIntoTheWild :: AgendaCard ExpeditionIntoTheWild
expeditionIntoTheWild = agenda (1, A) ExpeditionIntoTheWild Cards.expeditionIntoTheWild (Static 6)

instance RunMessage ExpeditionIntoTheWild where
  runMessage msg a@(ExpeditionIntoTheWild attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> ExpeditionIntoTheWild <$> runMessage msg attrs
