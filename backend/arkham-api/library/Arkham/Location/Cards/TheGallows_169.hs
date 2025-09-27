module Arkham.Location.Cards.TheGallows_169 (theGallows_169) where

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Witch))

newtype TheGallows_169 = TheGallows_169 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGallows_169 :: LocationCard TheGallows_169
theGallows_169 = location TheGallows_169 Cards.theGallows_169 3 (Static 0)

instance HasModifiersFor TheGallows_169 where
  getModifiersFor (TheGallows_169 a) = whenRevealed a do
    witchCount <- selectCount $ EnemyWithTrait Witch
    modifySelf a [ShroudModifier witchCount | witchCount > 0]

instance RunMessage TheGallows_169 where
  runMessage msg l@(TheGallows_169 attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.theGallowsSpectral_169
      pure l
    _ -> TheGallows_169 <$> liftRunMessage msg attrs
