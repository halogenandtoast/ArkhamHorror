module Arkham.Agenda.Cards.CityOfBlood
  ( CityOfBlood(..)
  , cityOfBlood
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype CityOfBlood = CityOfBlood AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfBlood :: AgendaCard CityOfBlood
cityOfBlood = agenda (4, A) CityOfBlood Cards.cityOfBlood (Static 4)

instance RunMessage CityOfBlood where
  runMessage msg a@(CityOfBlood attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> CityOfBlood <$> runMessage msg attrs
