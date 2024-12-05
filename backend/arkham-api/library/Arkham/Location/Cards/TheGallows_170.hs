module Arkham.Location.Cards.TheGallows_170 (theGallows_170, TheGallows_170 (..)) where

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Trait (Trait (Geist))

newtype TheGallows_170 = TheGallows_170 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGallows_170 :: LocationCard TheGallows_170
theGallows_170 = location TheGallows_170 Cards.theGallows_170 0 (Static 0)

instance HasModifiersFor TheGallows_170 where
  getModifiersFor (TheGallows_170 a) = whenRevealed a do
    geistCount <- selectCount $ EnemyWithTrait Geist
    modifySelf a [ShroudModifier geistCount | geistCount > 0]

instance RunMessage TheGallows_170 where
  runMessage msg l@(TheGallows_170 attrs) = runQueueT $ case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.theGallowsSpectral_170
      push $ ReplaceLocation attrs.id spectral Swap
      pure l
    _ -> TheGallows_170 <$> liftRunMessage msg attrs
