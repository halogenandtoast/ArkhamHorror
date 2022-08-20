module Arkham.Agenda.Cards.SomethingStirs
  ( SomethingStirs(..)
  , somethingStirs
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype SomethingStirs = SomethingStirs AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingStirs :: AgendaCard SomethingStirs
somethingStirs = agenda (1, A) SomethingStirs Cards.somethingStirs (Static 6)

instance RunMessage SomethingStirs where
  runMessage msg a@(SomethingStirs attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> SomethingStirs <$> runMessage msg attrs
