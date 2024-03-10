module Arkham.Location.Cards.MoonBeastGalley (
  moonBeastGalley,
  MoonBeastGalley (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MoonBeastGalley = MoonBeastGalley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonBeastGalley :: LocationCard MoonBeastGalley
moonBeastGalley = location MoonBeastGalley Cards.moonBeastGalley 0 (Static 0)

instance HasAbilities MoonBeastGalley where
  getAbilities (MoonBeastGalley attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage MoonBeastGalley where
  runMessage msg (MoonBeastGalley attrs) =
    MoonBeastGalley <$> runMessage msg attrs
