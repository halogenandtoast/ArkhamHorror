module Arkham.Location.Cards.LightingBox
  ( lightingBox
  , LightingBox(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Target

newtype LightingBox = LightingBox LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lightingBox :: LocationCard LightingBox
lightingBox =
  location LightingBox Cards.lightingBox 4 (PerPlayer 1) Plus [Triangle]

instance HasModifiersFor env LightingBox where
  getModifiersFor _ (InvestigatorTarget iid) (LightingBox attrs)
    | iid `on` attrs = pure $ toModifiers attrs [IncreaseCostOf AnyCard 2]
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage env LightingBox where
  runMessage msg (LightingBox attrs) = LightingBox <$> runMessage msg attrs
