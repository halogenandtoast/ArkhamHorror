module Arkham.Homebrew.TheSymphonyOfErichZann.Agendas.Overture (overture) where

import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Treacheries
import Arkham.Matcher

newtype Overture = Overture AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Maximum 1 [[Music]] treachery next to the agenda deck." -- the cap itself is
-- read off the agenda's stage by @Helpers.musicMaximum@.
overture :: AgendaCard Overture
overture = agenda (1, A) Overture Cards.overture (Static 6)

instance RunMessage Overture where
  runMessage msg a@(Overture attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      entranceHall <- selectJust $ locationIs Locations.entranceHall
      ears <- getSetAsideCardsMatching (cardIs Enemies.earsOfTheVoid)
      heard <- getSetAsideCardsMatching (cardIs Treacheries.heardBySomething)

      -- "Spawn a copy of Ears of the Void that has been set aside at the
      -- Entrance Hall. Shuffle the other copy and all copies of Heard by
      -- Something that were set aside into the encounter deck, along with the
      -- encounter discard pile."
      case ears of
        [] -> when (notNull heard) $ shuffleCardsIntoDeck Deck.EncounterDeck heard
        (spawned : rest) -> do
          createEnemyAt_ spawned entranceHall
          let shuffledBack = rest <> heard
          when (notNull shuffledBack) $ shuffleCardsIntoDeck Deck.EncounterDeck shuffledBack

      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> Overture <$> liftRunMessage msg attrs
