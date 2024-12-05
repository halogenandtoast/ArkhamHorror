module Arkham.Location.Cards.Easttown where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (easttown)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Easttown = Easttown LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

easttown :: LocationCard Easttown
easttown = location Easttown Cards.easttown 2 (PerPlayer 1)

instance HasModifiersFor Easttown where
  getModifiersFor (Easttown a) =
    whenRevealed a $ modifySelect a (investigatorAt a) [ReduceCostOf (#asset <> #ally) 2]

instance RunMessage Easttown where
  runMessage msg (Easttown attrs) = Easttown <$> runMessage msg attrs
