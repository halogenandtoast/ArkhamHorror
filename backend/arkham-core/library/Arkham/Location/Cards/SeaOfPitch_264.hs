module Arkham.Location.Cards.SeaOfPitch_264
  ( seaOfPitch_264
  , SeaOfPitch_264(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SeaOfPitch_264 = SeaOfPitch_264 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfPitch_264 :: LocationCard SeaOfPitch_264
seaOfPitch_264 = location SeaOfPitch_264 Cards.seaOfPitch_264 0 (PerPlayer 1)

instance HasAbilities SeaOfPitch_264 where
  getAbilities (SeaOfPitch_264 attrs) =
    extendRevealed attrs []

instance RunMessage SeaOfPitch_264 where
  runMessage msg (SeaOfPitch_264 attrs) =
    SeaOfPitch_264 <$> runMessage msg attrs
