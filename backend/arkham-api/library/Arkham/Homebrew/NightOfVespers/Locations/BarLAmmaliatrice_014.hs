-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.BarLAmmaliatrice_014 (barLAmmaliatrice_014) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype BarLAmmaliatrice_014 = BarLAmmaliatrice_014 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

barLAmmaliatrice_014 :: LocationCard BarLAmmaliatrice_014
barLAmmaliatrice_014 = location BarLAmmaliatrice_014 Cards.barLAmmaliatrice_014 2 (PerPlayer 1)

instance RunMessage BarLAmmaliatrice_014 where
  runMessage msg (BarLAmmaliatrice_014 attrs) = BarLAmmaliatrice_014 <$> runMessage msg attrs
