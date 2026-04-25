module Arkham.Location.Cards.WaterfrontDistrict (waterfrontDistrict) where

import Arkham.Cost
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards (waterfrontDistrict)
import Arkham.Location.Import.Lifted

newtype WaterfrontDistrict = WaterfrontDistrict LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

waterfrontDistrict :: LocationCard WaterfrontDistrict
waterfrontDistrict = location WaterfrontDistrict Cards.waterfrontDistrict 1 (PerPlayer 1)

instance HasModifiersFor WaterfrontDistrict where
  getModifiersFor (WaterfrontDistrict a) = whenRevealed a $ modifySelf a [AdditionalCostToInvestigate (ActionCost 1)]

instance RunMessage WaterfrontDistrict where
  runMessage msg (WaterfrontDistrict attrs) = WaterfrontDistrict <$> runMessage msg attrs
