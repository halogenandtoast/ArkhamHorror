module Arkham.Location.Cards.MysteriousStairs_184 (
  mysteriousStairs_184,
  MysteriousStairs_184 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MysteriousStairs_184 = MysteriousStairs_184 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_184 :: LocationCard MysteriousStairs_184
mysteriousStairs_184 = location MysteriousStairs_184 Cards.mysteriousStairs_184 0 (Static 0)

instance HasAbilities MysteriousStairs_184 where
  getAbilities (MysteriousStairs_184 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage MysteriousStairs_184 where
  runMessage msg (MysteriousStairs_184 attrs) =
    MysteriousStairs_184 <$> runMessage msg attrs
