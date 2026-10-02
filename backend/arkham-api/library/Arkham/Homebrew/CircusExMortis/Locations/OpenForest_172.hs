module Arkham.Homebrew.CircusExMortis.Locations.OpenForest_172 (openForest_172) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Locations.OpenForest_170 (cannotTriggerFreeAbilitiesWhile)
import Arkham.Location.Import.Lifted

newtype OpenForest_172 = OpenForest_172 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

openForest_172 :: LocationCard OpenForest_172
openForest_172 = location OpenForest_172 Cards.openForest_172 2 (PerPlayer 1)

instance HasModifiersFor OpenForest_172 where
  getModifiersFor (OpenForest_172 a) = cannotTriggerFreeAbilitiesWhile #fight a

instance RunMessage OpenForest_172 where
  runMessage msg (OpenForest_172 attrs) = runQueueT $ OpenForest_172 <$> liftRunMessage msg attrs
