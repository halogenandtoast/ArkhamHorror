module Arkham.Location.Cards.HallsOfPnakotusNorthernCorridors
  ( hallsOfPnakotusNorthernCorridors
  , HallsOfPnakotusNorthernCorridors(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HallsOfPnakotusNorthernCorridors = HallsOfPnakotusNorthernCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusNorthernCorridors
  :: LocationCard HallsOfPnakotusNorthernCorridors
hallsOfPnakotusNorthernCorridors = locationWith
  HallsOfPnakotusNorthernCorridors
  Cards.hallsOfPnakotusNorthernCorridors
  3
  (Static 1)
  (labelL .~ "hallsOfPnakotusNorthernCorridors")

instance HasAbilities HallsOfPnakotusNorthernCorridors where
  getAbilities (HallsOfPnakotusNorthernCorridors attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage HallsOfPnakotusNorthernCorridors where
  runMessage msg (HallsOfPnakotusNorthernCorridors attrs) =
    HallsOfPnakotusNorthernCorridors <$> runMessage msg attrs
