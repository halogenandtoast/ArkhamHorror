module Arkham.Location.Cards.SeaOfPitch_265
  ( seaOfPitch_265
  , SeaOfPitch_265(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SeaOfPitch_265 = SeaOfPitch_265 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfPitch_265 :: LocationCard SeaOfPitch_265
seaOfPitch_265 = location SeaOfPitch_265 Cards.seaOfPitch_265 0 (PerPlayer 1)

instance HasAbilities SeaOfPitch_265 where
  getAbilities (SeaOfPitch_265 attrs) =
    extendRevealed attrs []

instance RunMessage SeaOfPitch_265 where
  runMessage msg (SeaOfPitch_265 attrs) =
    SeaOfPitch_265 <$> runMessage msg attrs
