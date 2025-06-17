module Arkham.Location.Cards.ArtGalleryTheMidwinterGala (artGalleryTheMidwinterGala) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ArtGalleryTheMidwinterGala = ArtGalleryTheMidwinterGala LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Art GalleryTheMidwinterGala' from The Midwinter Gala (#71009).
artGalleryTheMidwinterGala :: LocationCard ArtGalleryTheMidwinterGala
artGalleryTheMidwinterGala = location ArtGalleryTheMidwinterGala Cards.artGalleryTheMidwinterGala 3 (PerPlayer 1)

instance HasAbilities ArtGalleryTheMidwinterGala where
  getAbilities (ArtGalleryTheMidwinterGala a) =
    extendRevealed1 a $ groupLimit PerRound $ restricted a 1 Here actionAbility

instance RunMessage ArtGalleryTheMidwinterGala where
  runMessage msg l@(ArtGalleryTheMidwinterGala attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs 2
      gainResources iid (attrs.ability 1) 2
      pure l
    _ -> ArtGalleryTheMidwinterGala <$> liftRunMessage msg attrs
