module Arkham.Location.Cards.SeaOfPitch_262 (
  seaOfPitch_262,
  SeaOfPitch_262 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SeaOfPitch_262 = SeaOfPitch_262 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfPitch_262 :: LocationCard SeaOfPitch_262
seaOfPitch_262 = location SeaOfPitch_262 Cards.seaOfPitch_262 0 (PerPlayer 1)

instance HasAbilities SeaOfPitch_262 where
  getAbilities (SeaOfPitch_262 attrs) =
    extendRevealed attrs []

instance RunMessage SeaOfPitch_262 where
  runMessage msg (SeaOfPitch_262 attrs) =
    SeaOfPitch_262 <$> runMessage msg attrs
