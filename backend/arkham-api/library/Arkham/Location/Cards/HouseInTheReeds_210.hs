module Arkham.Location.Cards.HouseInTheReeds_210 (houseInTheReeds_210, HouseInTheReeds_210 (..)) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (houseInTheReeds_210)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (withDrawCardUnderneathAction)
import Arkham.Matcher

newtype HouseInTheReeds_210 = HouseInTheReeds_210 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

houseInTheReeds_210 :: LocationCard HouseInTheReeds_210
houseInTheReeds_210 = location HouseInTheReeds_210 Cards.houseInTheReeds_210 2 (PerPlayer 1)

instance HasModifiersFor HouseInTheReeds_210 where
  getModifiersFor (HouseInTheReeds_210 a) =
    whenRevealed a $ modifySelect a (investigatorAt a) [CannotPlay #event]

instance HasAbilities HouseInTheReeds_210 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage HouseInTheReeds_210 where
  runMessage msg (HouseInTheReeds_210 attrs) = HouseInTheReeds_210 <$> runMessage msg attrs
