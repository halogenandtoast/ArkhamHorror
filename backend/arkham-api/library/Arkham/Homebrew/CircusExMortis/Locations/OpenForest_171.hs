module Arkham.Homebrew.CircusExMortis.Locations.OpenForest_171 (openForest_171) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Locations.OpenForest_170 (cannotTriggerFreeAbilitiesWhile)
import Arkham.Location.Import.Lifted

newtype OpenForest_171 = OpenForest_171 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

openForest_171 :: LocationCard OpenForest_171
openForest_171 = location OpenForest_171 Cards.openForest_171 2 (PerPlayer 1)

instance HasModifiersFor OpenForest_171 where
  getModifiersFor (OpenForest_171 a) = cannotTriggerFreeAbilitiesWhile #evade a

instance RunMessage OpenForest_171 where
  runMessage msg (OpenForest_171 attrs) = runQueueT $ OpenForest_171 <$> liftRunMessage msg attrs
