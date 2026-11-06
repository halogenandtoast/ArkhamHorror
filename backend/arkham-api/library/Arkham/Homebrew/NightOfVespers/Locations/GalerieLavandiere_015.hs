-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.GalerieLavandiere_015 (galerieLavandiere_015) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GalerieLavandiere_015 = GalerieLavandiere_015 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

galerieLavandiere_015 :: LocationCard GalerieLavandiere_015
galerieLavandiere_015 = symbolLabel $ location GalerieLavandiere_015 Cards.galerieLavandiere_015 3 (PerPlayer 1)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities GalerieLavandiere_015 where
  getAbilities (GalerieLavandiere_015 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage GalerieLavandiere_015 where
  runMessage msg l@(GalerieLavandiere_015 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hotelLobby_019
          , Cards.hotelLobby_020
          , Cards.luxurySuites_021
          , Cards.luxurySuites_022
          ]
      pure l
    _ -> GalerieLavandiere_015 <$> liftRunMessage msg attrs
