module Arkham.Homebrew.AgesUnwound.Locations.FeaturelessStreets (featurelessStreets) where

import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype FeaturelessStreets = FeaturelessStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Scenario VI's starting location on the /the Myriad harnessed the power of
another realm/ branch; otherwise it is removed from the game during setup. It
prints no symbol and no connections: it is where the school /should/ be, and act
1a removes it as the school ripples into view.

No printed abilities, so the module exists only to register the builder --- a def
with no behaviour module @error@s the first time the card is put into play.
-}
featurelessStreets :: LocationCard FeaturelessStreets
featurelessStreets = location FeaturelessStreets Cards.featurelessStreets 3 (PerPlayer 2)

instance RunMessage FeaturelessStreets where
  runMessage msg (FeaturelessStreets attrs) = FeaturelessStreets <$> runMessage msg attrs
