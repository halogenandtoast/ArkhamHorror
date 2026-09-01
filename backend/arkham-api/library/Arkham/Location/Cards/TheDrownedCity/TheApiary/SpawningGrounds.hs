module Arkham.Location.Cards.TheDrownedCity.TheApiary.SpawningGrounds (spawningGrounds) where

import Arkham.Deck qualified as Deck
import Arkham.Location.CardDefs.TheDrownedCity.TheApiary qualified as Cards
import Arkham.Location.Import.Lifted

newtype SpawningGrounds = SpawningGrounds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

spawningGrounds :: LocationCard SpawningGrounds
spawningGrounds = location SpawningGrounds Cards.spawningGrounds 3 (PerPlayer 2)

instance RunMessage SpawningGrounds where
  runMessage msg (SpawningGrounds attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      -- Shuffle the encounter discard pile into the encounter deck.
      shuffleEncounterDiscardBackIn
      -- Find the set-aside Lost Campsite and Churning Chasm and shuffle them
      -- into the bottom 10 cards of the encounter deck.
      lostCampsite <- getSetAsideCard Cards.lostCampsite
      churningChasm <- getSetAsideCard Cards.churningChasm
      shuffleCardsIntoBottomOfDeck Deck.EncounterDeck 10 [lostCampsite, churningChasm]
      SpawningGrounds <$> liftRunMessage msg attrs
    _ -> SpawningGrounds <$> liftRunMessage msg attrs
