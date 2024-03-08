module Arkham.Location.Cards.MysteriousStairs_183 (
  mysteriousStairs_183,
  MysteriousStairs_183 (..),
)
where

import Arkham.Prelude

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MysteriousStairs_183 = MysteriousStairs_183 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_183 :: LocationCard MysteriousStairs_183
mysteriousStairs_183 =
  locationWith
    MysteriousStairs_183
    Cards.mysteriousStairs_183
    0
    (Static 0)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities MysteriousStairs_183 where
  getAbilities (MysteriousStairs_183 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage MysteriousStairs_183 where
  runMessage msg (MysteriousStairs_183 attrs) =
    MysteriousStairs_183 <$> runMessage msg attrs
