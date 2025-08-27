module Arkham.Agenda.Cards.HallsOfStMarys (hallsOfStMarys) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.EncounterSet
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Story
import Arkham.Placement
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Cards qualified as Treacheries

newtype HallsOfStMarys = HallsOfStMarys AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfStMarys :: AgendaCard HallsOfStMarys
hallsOfStMarys = agenda (1, A) HallsOfStMarys Cards.hallsOfStMarys (Static 2)

instance RunMessage HallsOfStMarys where
  runMessage msg a@(HallsOfStMarys attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      spiders <- map toCard <$> gatherEncounterSet EncounterSet.Spiders
      outbreaks <- getSetAsideCardsMatching $ cardIs Treacheries.outbreak
      theInfestationBegins <- getSetAsideCard Stories.theInfestationBegins
      lead <- getLead
      shuffleCardsIntoDeck Deck.EncounterDeck (spiders <> outbreaks)
      resolveStoryWithPlacement lead theInfestationBegins Global
      advanceAgendaDeck attrs
      pure a
    _ -> HallsOfStMarys <$> liftRunMessage msg attrs
