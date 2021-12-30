module Arkham.Location.Cards.GardensOfLuxembourg
  ( gardensOfLuxembourg
  , GardensOfLuxembourg(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype GardensOfLuxembourg = GardensOfLuxembourg LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gardensOfLuxembourg :: LocationCard GardensOfLuxembourg
gardensOfLuxembourg = location
  GardensOfLuxembourg
  Cards.gardensOfLuxembourg
  3
  (PerPlayer 1)
  Star
  [Circle, Heart, Plus]

instance HasAbilities GardensOfLuxembourg where
  getAbilities (GardensOfLuxembourg attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env GardensOfLuxembourg where
  runMessage msg (GardensOfLuxembourg attrs) =
    GardensOfLuxembourg <$> runMessage msg attrs
