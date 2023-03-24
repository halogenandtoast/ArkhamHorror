module Arkham.Agenda.Cards.OverTheThreshold
  ( OverTheThreshold(..)
  , overTheThreshold
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype OverTheThreshold = OverTheThreshold AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overTheThreshold :: AgendaCard OverTheThreshold
overTheThreshold = agenda (2, A) OverTheThreshold Cards.overTheThreshold (Static 11)

instance RunMessage OverTheThreshold where
  runMessage msg a@(OverTheThreshold attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> OverTheThreshold <$> runMessage msg attrs
