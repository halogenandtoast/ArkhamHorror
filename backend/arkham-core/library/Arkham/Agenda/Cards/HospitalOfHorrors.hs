module Arkham.Agenda.Cards.HospitalOfHorrors (
  HospitalOfHorrors (..),
  hospitalOfHorrors,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype HospitalOfHorrors = HospitalOfHorrors AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hospitalOfHorrors :: AgendaCard HospitalOfHorrors
hospitalOfHorrors = agenda (3, A) HospitalOfHorrors Cards.hospitalOfHorrors (Static 8)

instance RunMessage HospitalOfHorrors where
  runMessage msg a@(HospitalOfHorrors attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> HospitalOfHorrors <$> runMessage msg attrs
