module Arkham.Location.Cards.SpawningGrounds (spawningGrounds) where

import Arkham.Deck qualified as Deck
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck, getEncounterDiscard)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (RegularEncounterDeck))

newtype SpawningGrounds = SpawningGrounds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spawningGrounds :: LocationCard SpawningGrounds
spawningGrounds = location SpawningGrounds Cards.spawningGrounds 3 (Static 2)

instance RunMessage SpawningGrounds where
  runMessage msg (SpawningGrounds attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      -- Shuffle the encounter discard pile into the encounter deck.
      shuffleEncounterDiscardBackIn
      -- Find the set-aside Lost Campsite and Churning Chasm and shuffle them
      -- into the bottom 10 cards of the encounter deck.
      lostCampsite <- getSetAsideCard Cards.lostCampsite
      churningChasm <- getSetAsideCard Cards.churningChasm
      let cards = [lostCampsite, churningChasm]
      -- Account for the discard that is about to be shuffled back in so that the
      -- "bottom 10" is measured against the full deck.
      deckSize <- length . unDeck <$> getEncounterDeck
      discardSize <- length <$> getEncounterDiscard RegularEncounterDeck
      let total = deckSize + discardSize
      if total <= 10
        then shuffleCardsIntoDeck Deck.EncounterDeck cards
        else do
          -- Rotate the bottom 10 cards to the top, shuffle the set-aside cards
          -- into them, then rotate the resulting 12 cards back to the bottom.
          push $ MoveTopOfDeckToBottom (toSource attrs) Deck.EncounterDeck (total - 10)
          push $ ShuffleCardsIntoTopOfDeck Deck.EncounterDeck 10 cards
          push $ MoveTopOfDeckToBottom (toSource attrs) Deck.EncounterDeck 12
      SpawningGrounds <$> liftRunMessage msg attrs
    _ -> SpawningGrounds <$> liftRunMessage msg attrs
