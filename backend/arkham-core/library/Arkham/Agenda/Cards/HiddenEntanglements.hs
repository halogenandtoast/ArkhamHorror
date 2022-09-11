module Arkham.Agenda.Cards.HiddenEntanglements
  ( HiddenEntanglements(..)
  , hiddenEntanglements
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype HiddenEntanglements = HiddenEntanglements AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenEntanglements :: AgendaCard HiddenEntanglements
hiddenEntanglements =
  agenda (3, A) HiddenEntanglements Cards.hiddenEntanglements (Static 12)

instance RunMessage HiddenEntanglements where
  runMessage msg a@(HiddenEntanglements attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> HiddenEntanglements <$> runMessage msg attrs
