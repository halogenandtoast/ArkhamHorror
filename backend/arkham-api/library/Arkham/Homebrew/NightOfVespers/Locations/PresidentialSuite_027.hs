-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.PresidentialSuite_027 (presidentialSuite_027) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype PresidentialSuite_027 = PresidentialSuite_027 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

presidentialSuite_027 :: LocationCard PresidentialSuite_027
presidentialSuite_027 = location PresidentialSuite_027 Cards.presidentialSuite_027 4 (PerPlayer 2)

instance RunMessage PresidentialSuite_027 where
  runMessage msg (PresidentialSuite_027 attrs) = PresidentialSuite_027 <$> runMessage msg attrs
