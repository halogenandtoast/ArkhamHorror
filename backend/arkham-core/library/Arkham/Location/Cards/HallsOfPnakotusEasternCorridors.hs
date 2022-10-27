module Arkham.Location.Cards.HallsOfPnakotusEasternCorridors
  ( hallsOfPnakotusEasternCorridors
  , HallsOfPnakotusEasternCorridors(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HallsOfPnakotusEasternCorridors = HallsOfPnakotusEasternCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusEasternCorridors :: LocationCard HallsOfPnakotusEasternCorridors
hallsOfPnakotusEasternCorridors = locationWith
  HallsOfPnakotusEasternCorridors
  Cards.hallsOfPnakotusEasternCorridors
  3
  (Static 1)
  (labelL .~ "hallsOfPnakotusEasternCorridors")

instance HasAbilities HallsOfPnakotusEasternCorridors where
  getAbilities (HallsOfPnakotusEasternCorridors attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage HallsOfPnakotusEasternCorridors where
  runMessage msg (HallsOfPnakotusEasternCorridors attrs) =
    HallsOfPnakotusEasternCorridors <$> runMessage msg attrs
