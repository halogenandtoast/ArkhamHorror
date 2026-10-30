-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.BarLAmmaliatrice_013 (barLAmmaliatrice_013) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype BarLAmmaliatrice_013 = BarLAmmaliatrice_013 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

barLAmmaliatrice_013 :: LocationCard BarLAmmaliatrice_013
barLAmmaliatrice_013 = symbolLabel $ location BarLAmmaliatrice_013 Cards.barLAmmaliatrice_013 2 (PerPlayer 1)

instance RunMessage BarLAmmaliatrice_013 where
  runMessage msg (BarLAmmaliatrice_013 attrs) = BarLAmmaliatrice_013 <$> runMessage msg attrs
