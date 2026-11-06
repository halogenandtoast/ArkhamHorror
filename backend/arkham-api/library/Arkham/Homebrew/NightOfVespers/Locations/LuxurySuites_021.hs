-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.LuxurySuites_021 (luxurySuites_021) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LuxurySuites_021 = LuxurySuites_021 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luxurySuites_021 :: LocationCard LuxurySuites_021
luxurySuites_021 = symbolLabel $ location LuxurySuites_021 Cards.luxurySuites_021 2 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities LuxurySuites_021 where
  getAbilities (LuxurySuites_021 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage LuxurySuites_021 where
  runMessage msg l@(LuxurySuites_021 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.conservatory
          , Cards.hotelRooftop
          ]
      pure l
    _ -> LuxurySuites_021 <$> liftRunMessage msg attrs
