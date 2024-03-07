module Arkham.Location.Cards.MysteriousStairs_185 (
  mysteriousStairs_185,
  MysteriousStairs_185 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MysteriousStairs_185 = MysteriousStairs_185 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_185 :: LocationCard MysteriousStairs_185
mysteriousStairs_185 = location MysteriousStairs_185 Cards.mysteriousStairs_185 0 (Static 0)

instance HasAbilities MysteriousStairs_185 where
  getAbilities (MysteriousStairs_185 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage MysteriousStairs_185 where
  runMessage msg (MysteriousStairs_185 attrs) =
    MysteriousStairs_185 <$> runMessage msg attrs
