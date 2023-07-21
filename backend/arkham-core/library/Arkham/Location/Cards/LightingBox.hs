module Arkham.Location.Cards.LightingBox (
  lightingBox,
  LightingBox (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype LightingBox = LightingBox LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lightingBox :: LocationCard LightingBox
lightingBox = location LightingBox Cards.lightingBox 4 (PerPlayer 1)

instance HasModifiersFor LightingBox where
  getModifiersFor (InvestigatorTarget iid) (LightingBox attrs)
    | iid `on` attrs = pure $ toModifiers attrs [IncreaseCostOf AnyCard 2]
  getModifiersFor _ _ = pure []

instance RunMessage LightingBox where
  runMessage msg (LightingBox attrs) = LightingBox <$> runMessage msg attrs
