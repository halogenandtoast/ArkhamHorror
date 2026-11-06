-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.BarLAmmaliatrice_013 (barLAmmaliatrice_013) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BarLAmmaliatrice_013 = BarLAmmaliatrice_013 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barLAmmaliatrice_013 :: LocationCard BarLAmmaliatrice_013
barLAmmaliatrice_013 = symbolLabel $ location BarLAmmaliatrice_013 Cards.barLAmmaliatrice_013 2 (PerPlayer 1)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities BarLAmmaliatrice_013 where
  getAbilities (BarLAmmaliatrice_013 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage BarLAmmaliatrice_013 where
  runMessage msg l@(BarLAmmaliatrice_013 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.galerieLavandiere_015
          , Cards.galerieLavandiere_016
          , Cards.mezzanineElevators_023
          , Cards.mezzanineElevators_024
          ]
      pure l
    _ -> BarLAmmaliatrice_013 <$> liftRunMessage msg attrs
