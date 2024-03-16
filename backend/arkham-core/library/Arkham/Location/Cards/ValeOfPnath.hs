module Arkham.Location.Cards.ValeOfPnath (
  valeOfPnath,
  ValeOfPnath (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ValeOfPnath = ValeOfPnath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeOfPnath :: LocationCard ValeOfPnath
valeOfPnath = location ValeOfPnath Cards.valeOfPnath 4 (PerPlayer 1)

instance HasAbilities ValeOfPnath where
  getAbilities (ValeOfPnath attrs) =
    extendRevealed attrs []

instance RunMessage ValeOfPnath where
  runMessage msg (ValeOfPnath attrs) =
    ValeOfPnath <$> runMessage msg attrs
