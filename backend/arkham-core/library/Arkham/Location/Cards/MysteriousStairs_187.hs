module Arkham.Location.Cards.MysteriousStairs_187 (
  mysteriousStairs_187,
  MysteriousStairs_187 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MysteriousStairs_187 = MysteriousStairs_187 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_187 :: LocationCard MysteriousStairs_187
mysteriousStairs_187 = location MysteriousStairs_187 Cards.mysteriousStairs_187 0 (Static 0)

instance HasAbilities MysteriousStairs_187 where
  getAbilities (MysteriousStairs_187 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage MysteriousStairs_187 where
  runMessage msg (MysteriousStairs_187 attrs) =
    MysteriousStairs_187 <$> runMessage msg attrs
