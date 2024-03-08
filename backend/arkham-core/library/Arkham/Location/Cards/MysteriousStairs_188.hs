module Arkham.Location.Cards.MysteriousStairs_188 (
  mysteriousStairs_188,
  MysteriousStairs_188 (..),
)
where

import Arkham.Prelude

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MysteriousStairs_188 = MysteriousStairs_188 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_188 :: LocationCard MysteriousStairs_188
mysteriousStairs_188 =
  locationWith
    MysteriousStairs_188
    Cards.mysteriousStairs_188
    0
    (Static 0)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities MysteriousStairs_188 where
  getAbilities (MysteriousStairs_188 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage MysteriousStairs_188 where
  runMessage msg (MysteriousStairs_188 attrs) =
    MysteriousStairs_188 <$> runMessage msg attrs
