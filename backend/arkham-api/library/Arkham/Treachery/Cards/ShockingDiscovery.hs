module Arkham.Treachery.Cards.ShockingDiscovery (shockingDiscovery) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (getBatchId)

newtype ShockingDiscovery = ShockingDiscovery TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shockingDiscovery :: TreacheryCard ShockingDiscovery
shockingDiscovery = treachery ShockingDiscovery Cards.shockingDiscovery

instance HasAbilities ShockingDiscovery where
  getAbilities (ShockingDiscovery x) = [mkAbility x 1 $ forced (AmongSearchedCards You)]

-- Forced - When you search your deck and this card is among the searched
-- cards: Discard it. Cancel the search and all of its effects. Shuffle the
-- searched deck. Draw the top card of the encounter deck.

instance RunMessage ShockingDiscovery where
  runMessage msg t@(ShockingDiscovery attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      canShuffleDeck <- getCanShuffleDeck iid
      hasEncounterDeck <- can.target.encounterDeck iid
      if canShuffleDeck
        then shuffleIntoDeck iid attrs
        else when hasEncounterDeck $ drawEncounterCards iid attrs 1
      pure t
    InSearch (UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _) -> do
      let card = fromJustNote "is player card" $ preview _PlayerCard (toCard attrs)
      push $ RemoveCardFromSearch iid (toCardId card)
      addToDiscard iid (only card)
      cancelBatch batchId
      push $ CancelSearch (toTarget iid) -- shuffles the deck
      drawEncounterCards iid attrs 1
      pure t
    _ -> ShockingDiscovery <$> liftRunMessage msg attrs
