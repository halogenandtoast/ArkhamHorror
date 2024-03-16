module Arkham.Location.Cards.SeaOfPitch_263 (
  seaOfPitch_263,
  SeaOfPitch_263 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SeaOfPitch_263 = SeaOfPitch_263 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfPitch_263 :: LocationCard SeaOfPitch_263
seaOfPitch_263 = location SeaOfPitch_263 Cards.seaOfPitch_263 0 (PerPlayer 1)

instance HasAbilities SeaOfPitch_263 where
  getAbilities (SeaOfPitch_263 attrs) =
    extendRevealed attrs []

instance RunMessage SeaOfPitch_263 where
  runMessage msg (SeaOfPitch_263 attrs) =
    SeaOfPitch_263 <$> runMessage msg attrs
