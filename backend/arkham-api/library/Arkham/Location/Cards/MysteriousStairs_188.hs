module Arkham.Location.Cards.MysteriousStairs_188 (mysteriousStairs_188, MysteriousStairs_188 (..)) where

import Arkham.Cost
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MysteriousStairs_188 = MysteriousStairs_188 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mysteriousStairs_188 :: LocationCard MysteriousStairs_188
mysteriousStairs_188 =
  locationWith MysteriousStairs_188 Cards.mysteriousStairs_188 3 (Static 0)
    $ connectsToL
    .~ setFromList [Above, Below]

instance HasModifiersFor MysteriousStairs_188 where
  getModifiersFor (MysteriousStairs_188 a) =
    modifySelf
      a
      [AdditionalCostToLeave $ SameSkillIconCost 2, AdditionalCostToResign $ SameSkillIconCost 2]

instance RunMessage MysteriousStairs_188 where
  runMessage msg (MysteriousStairs_188 attrs) =
    MysteriousStairs_188 <$> runMessage msg attrs
