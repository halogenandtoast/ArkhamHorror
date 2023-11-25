module Arkham.Agenda.Cards.HallsOfStMarys (
  HallsOfStMarys (..),
  hallsOfStMarys,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Cards qualified as Treacheries

newtype HallsOfStMarys = HallsOfStMarys AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfStMarys :: AgendaCard HallsOfStMarys
hallsOfStMarys = agenda (1, A) HallsOfStMarys Cards.hallsOfStMarys (Static 2)

instance RunMessage HallsOfStMarys where
  runMessage msg a@(HallsOfStMarys attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        spiders <- map toCard <$> gatherEncounterSet EncounterSet.Spiders
        outbreaks <- getSetAsideCardsMatching $ cardIs Treacheries.outbreak
        theInfestationBegins <- getSetAsideCard Stories.theInfestationBegins
        lead <- getLead
        pushAll
          [ ShuffleCardsIntoDeck Deck.EncounterDeck (spiders <> outbreaks)
          , ReadStoryWithPlacement lead theInfestationBegins ResolveIt Nothing Global
          , advanceAgendaDeck attrs
          ]
        pure a
      _ -> HallsOfStMarys <$> runMessage msg attrs
