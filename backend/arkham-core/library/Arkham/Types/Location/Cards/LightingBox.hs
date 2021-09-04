module Arkham.Types.Location.Cards.LightingBox
  ( lightingBox
  , LightingBox(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.Target

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
