module Arkham.Location.Cards.FaubourgMarigny (FaubourgMarigny (..), faubourgMarigny) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (faubourgMarigny)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FaubourgMarigny = FaubourgMarigny LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

faubourgMarigny :: LocationCard FaubourgMarigny
faubourgMarigny = location FaubourgMarigny Cards.faubourgMarigny 4 (Static 0)

instance HasModifiersFor FaubourgMarigny where
  getModifiersFor (FaubourgMarigny a) =
    whenRevealed a $ modifySelect a (investigatorAt a) [ReduceCostOf #asset 1]

instance HasAbilities FaubourgMarigny where
  getAbilities (FaubourgMarigny a) = extendRevealed1 a $ locationResignAction a

instance RunMessage FaubourgMarigny where
  runMessage msg (FaubourgMarigny attrs) = FaubourgMarigny <$> runMessage msg attrs
