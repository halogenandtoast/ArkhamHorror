module Arkham.Location.Cards.HallsOfPnakotusWesternCorridors
  ( hallsOfPnakotusWesternCorridors
  , HallsOfPnakotusWesternCorridors(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HallsOfPnakotusWesternCorridors = HallsOfPnakotusWesternCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusWesternCorridors :: LocationCard HallsOfPnakotusWesternCorridors
hallsOfPnakotusWesternCorridors = locationWith
  HallsOfPnakotusWesternCorridors
  Cards.hallsOfPnakotusWesternCorridors
  3
  (Static 1)
  (labelL .~ "hallsOfPnakotusWesternCorridors")

instance HasAbilities HallsOfPnakotusWesternCorridors where
  getAbilities (HallsOfPnakotusWesternCorridors attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage HallsOfPnakotusWesternCorridors where
  runMessage msg (HallsOfPnakotusWesternCorridors attrs) =
    HallsOfPnakotusWesternCorridors <$> runMessage msg attrs
