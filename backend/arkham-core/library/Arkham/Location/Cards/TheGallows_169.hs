module Arkham.Location.Cards.TheGallows_169 (
  theGallows_169,
  TheGallows_169 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheGallows_169 = TheGallows_169 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGallows_169 :: LocationCard TheGallows_169
theGallows_169 = location TheGallows_169 Cards.theGallows_169 3 (Static 0)

instance HasAbilities TheGallows_169 where
  getAbilities (TheGallows_169 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage TheGallows_169 where
  runMessage msg (TheGallows_169 attrs) =
    TheGallows_169 <$> runMessage msg attrs
