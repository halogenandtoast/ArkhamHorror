module Arkham.Location.Cards.MysteriousStairs_186 (
  mysteriousStairs_186,
  MysteriousStairs_186 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MysteriousStairs_186 = MysteriousStairs_186 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_186 :: LocationCard MysteriousStairs_186
mysteriousStairs_186 = location MysteriousStairs_186 Cards.mysteriousStairs_186 0 (Static 0)

instance HasAbilities MysteriousStairs_186 where
  getAbilities (MysteriousStairs_186 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage MysteriousStairs_186 where
  runMessage msg (MysteriousStairs_186 attrs) =
    MysteriousStairs_186 <$> runMessage msg attrs
