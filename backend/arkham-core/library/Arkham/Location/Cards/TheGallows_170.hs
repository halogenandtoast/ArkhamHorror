module Arkham.Location.Cards.TheGallows_170 (
  theGallows_170,
  TheGallows_170 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (Geist))

newtype TheGallows_170 = TheGallows_170 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

theGallows_170 :: LocationCard TheGallows_170
theGallows_170 = location TheGallows_170 Cards.theGallows_170 0 (Static 0)

instance HasModifiersFor TheGallows_170 where
  getModifiersFor target (TheGallows_170 attrs) | isTarget attrs target = do
    geistCount <- selectCount $ EnemyWithTrait Geist
    pure $ toModifiers attrs [ShroudModifier geistCount | geistCount > 0]
  getModifiersFor _ _ = pure []

instance RunMessage TheGallows_170 where
  runMessage msg l@(TheGallows_170 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.theGallowsSpectral_170
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> TheGallows_170 <$> runMessage msg attrs
