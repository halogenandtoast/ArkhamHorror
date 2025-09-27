module Arkham.Treachery.Cards.GraveLight (graveLight) where

import Arkham.Card
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GraveLight = GraveLight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveLight :: TreacheryCard GraveLight
graveLight = treachery GraveLight Cards.graveLight

instance RunMessage GraveLight where
  runMessage msg t@(GraveLight attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      gainSurge attrs
      removeTreachery attrs
      let graveLightSpectral = lookupCard Cards.graveLightSpectral (toCardId attrs)
      replaceCard (toCardId attrs) graveLightSpectral
      shuffleCardsIntoDeck SpectralEncounterDeck [graveLightSpectral]
      pure t
    _ -> GraveLight <$> liftRunMessage msg attrs
