module Arkham.Agenda.Cards.TheRitualBeginsBlackStarsRise
  ( TheRitualBeginsBlackStarsRise
  , theRitualBeginsBlackStarsRise
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheRitualBeginsBlackStarsRise = TheRitualBeginsBlackStarsRise AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBeginsBlackStarsRise :: AgendaCard TheRitualBeginsBlackStarsRise
theRitualBeginsBlackStarsRise = agenda (1, A) TheRitualBeginsBlackStarsRise Cards.theRitualBeginsBlackStarsRise (Static 12)

instance RunMessage TheRitualBeginsBlackStarsRise where
  runMessage msg a@(TheRitualBeginsBlackStarsRise attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> TheRitualBeginsBlackStarsRise <$> runMessage msg attrs
