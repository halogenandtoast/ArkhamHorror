module Arkham.Treachery.Cards.GraveLightSpectral (graveLightSpectral) where

import Arkham.Card
import Arkham.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GraveLightSpectral = GraveLightSpectral TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveLightSpectral :: TreacheryCard GraveLightSpectral
graveLightSpectral = treachery GraveLightSpectral Cards.graveLightSpectral

instance RunMessage GraveLightSpectral where
  runMessage msg t@(GraveLightSpectral attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      removeTreachery attrs
      let graveLight = lookupCard Cards.graveLight (toCardId attrs)
      replaceCard (toCardId attrs) graveLight
      shuffleCardsIntoDeck EncounterDeck [graveLight]
      pure t
    _ -> GraveLightSpectral <$> liftRunMessage msg attrs
