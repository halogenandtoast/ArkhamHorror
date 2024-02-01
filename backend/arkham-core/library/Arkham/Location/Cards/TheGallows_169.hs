module Arkham.Location.Cards.TheGallows_169 (
  theGallows_169,
  TheGallows_169 (..),
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
import Arkham.Trait (Trait (Witch))

newtype TheGallows_169 = TheGallows_169 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

theGallows_169 :: LocationCard TheGallows_169
theGallows_169 = location TheGallows_169 Cards.theGallows_169 3 (Static 0)

instance HasModifiersFor TheGallows_169 where
  getModifiersFor target (TheGallows_169 a) | isTarget a target = do
    witchCount <- selectCount $ EnemyWithTrait Witch
    pure $ toModifiers a [ShroudModifier witchCount | witchCount > 0]
  getModifiersFor _ _ = pure []

instance RunMessage TheGallows_169 where
  runMessage msg l@(TheGallows_169 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.theGallowsSpectral_169
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> TheGallows_169 <$> runMessage msg attrs
