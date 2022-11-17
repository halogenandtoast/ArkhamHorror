module Arkham.Agenda.Cards.HorrificDescent
  ( HorrificDescent(..)
  , horrificDescent
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype HorrificDescent = HorrificDescent AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrificDescent :: AgendaCard HorrificDescent
horrificDescent =
  agenda (2, A) HorrificDescent Cards.horrificDescent (Static 3)

instance RunMessage HorrificDescent where
  runMessage msg a@(HorrificDescent attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> HorrificDescent <$> runMessage msg attrs
