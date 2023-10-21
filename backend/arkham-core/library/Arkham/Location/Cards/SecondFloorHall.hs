module Arkham.Location.Cards.SecondFloorHall
  ( secondFloorHall
  , SecondFloorHall(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SecondFloorHall = SecondFloorHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondFloorHall :: LocationCard SecondFloorHall
secondFloorHall = location SecondFloorHall Cards.secondFloorHall 2 (PerPlayer 1)

instance HasAbilities SecondFloorHall where
  getAbilities (SecondFloorHall attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage SecondFloorHall where
  runMessage msg (SecondFloorHall attrs) =
    SecondFloorHall <$> runMessage msg attrs
