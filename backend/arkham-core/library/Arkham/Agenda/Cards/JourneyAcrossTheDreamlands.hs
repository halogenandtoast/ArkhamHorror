module Arkham.Agenda.Cards.JourneyAcrossTheDreamlands (JourneyAcrossTheDreamlands (..), journeyAcrossTheDreamlands) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Prelude

newtype JourneyAcrossTheDreamlands = JourneyAcrossTheDreamlands AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

journeyAcrossTheDreamlands :: AgendaCard JourneyAcrossTheDreamlands
journeyAcrossTheDreamlands = agenda (1, A) JourneyAcrossTheDreamlands Cards.journeyAcrossTheDreamlands (Static 7)

instance RunMessage JourneyAcrossTheDreamlands where
  runMessage msg a@(JourneyAcrossTheDreamlands attrs) =
    case msg of
      AdvanceAgenda aid
        | aid == toId attrs && onSide B attrs ->
            a <$ pushAll [advanceAgendaDeck attrs]
      _ -> JourneyAcrossTheDreamlands <$> runMessage msg attrs
