module Arkham.Location.Cards.ReturnToHouseInTheReeds (returnToHouseInTheReeds) where

import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToHouseInTheReeds = ReturnToHouseInTheReeds LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToHouseInTheReeds :: LocationCard ReturnToHouseInTheReeds
returnToHouseInTheReeds =
  locationWith
    ReturnToHouseInTheReeds
    Cards.returnToHouseInTheReeds
    2
    (PerPlayer 1)
    (labelL .~ "houseInTheReeds")

instance HasModifiersFor ReturnToHouseInTheReeds where
  getModifiersFor (ReturnToHouseInTheReeds a) = modifySelect a (investigatorAt a) [CannotPlay #asset]

instance HasAbilities ReturnToHouseInTheReeds where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage ReturnToHouseInTheReeds where
  runMessage msg (ReturnToHouseInTheReeds attrs) = ReturnToHouseInTheReeds <$> runMessage msg attrs
