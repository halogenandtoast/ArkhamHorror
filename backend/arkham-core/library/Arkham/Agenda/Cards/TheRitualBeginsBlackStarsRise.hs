module Arkham.Agenda.Cards.TheRitualBeginsBlackStarsRise
  ( TheRitualBeginsBlackStarsRise
  , theRitualBeginsBlackStarsRise
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.Target

newtype TheRitualBeginsBlackStarsRise = TheRitualBeginsBlackStarsRise AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBeginsBlackStarsRise :: AgendaCard TheRitualBeginsBlackStarsRise
theRitualBeginsBlackStarsRise = agenda
  (1, C)
  TheRitualBeginsBlackStarsRise
  Cards.theRitualBeginsBlackStarsRise
  (Static 5)

instance HasModifiersFor TheRitualBeginsBlackStarsRise where
  getModifiersFor _ (AgendaTarget aid) (TheRitualBeginsBlackStarsRise attrs)
    | aid /= toId attrs = pure $ toModifiers attrs [DoNotCountDoom]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheRitualBeginsBlackStarsRise where
  runMessage msg a@(TheRitualBeginsBlackStarsRise attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide D attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> TheRitualBeginsBlackStarsRise <$> runMessage msg attrs
