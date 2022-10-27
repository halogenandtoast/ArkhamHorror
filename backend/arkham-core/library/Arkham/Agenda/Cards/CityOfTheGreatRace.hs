module Arkham.Agenda.Cards.CityOfTheGreatRace
  ( CityOfTheGreatRace(..)
  , cityOfTheGreatRace
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype CityOfTheGreatRace = CityOfTheGreatRace AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheGreatRace :: AgendaCard CityOfTheGreatRace
cityOfTheGreatRace =
  agenda (1, A) CityOfTheGreatRace Cards.cityOfTheGreatRace (Static 5)

instance RunMessage CityOfTheGreatRace where
  runMessage msg a@(CityOfTheGreatRace attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> CityOfTheGreatRace <$> runMessage msg attrs
