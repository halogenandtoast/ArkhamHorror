module Arkham.Agenda.Cards.HallsOfStMarys
  ( HallsOfStMarys(..)
  , hallsOfStMarys
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype HallsOfStMarys = HallsOfStMarys AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfStMarys :: AgendaCard HallsOfStMarys
hallsOfStMarys = agenda (1, A) HallsOfStMarys Cards.hallsOfStMarys (Static 2)

instance RunMessage HallsOfStMarys where
  runMessage msg a@(HallsOfStMarys attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> HallsOfStMarys <$> runMessage msg attrs
