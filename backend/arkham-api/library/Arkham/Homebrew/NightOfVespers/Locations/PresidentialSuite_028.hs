-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.PresidentialSuite_028 (presidentialSuite_028) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype PresidentialSuite_028 = PresidentialSuite_028 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

presidentialSuite_028 :: LocationCard PresidentialSuite_028
presidentialSuite_028 = symbolLabel $ location PresidentialSuite_028 Cards.presidentialSuite_028 4 (PerPlayer 2)

instance RunMessage PresidentialSuite_028 where
  runMessage msg (PresidentialSuite_028 attrs) = PresidentialSuite_028 <$> runMessage msg attrs
