-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.LuxurySuites_022 (luxurySuites_022) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LuxurySuites_022 = LuxurySuites_022 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luxurySuites_022 :: LocationCard LuxurySuites_022
luxurySuites_022 = symbolLabel $ location LuxurySuites_022 Cards.luxurySuites_022 2 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities LuxurySuites_022 where
  getAbilities (LuxurySuites_022 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage LuxurySuites_022 where
  runMessage msg l@(LuxurySuites_022 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.conservatory
          , Cards.hotelRooftop
          ]
      pure l
    _ -> LuxurySuites_022 <$> liftRunMessage msg attrs
