module Arkham.Location.Cards.EngineCar_175
  ( engineCar_175
  , EngineCar_175(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( engineCar_175 )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Projection

newtype EngineCar_175 = EngineCar_175 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

engineCar_175 :: LocationCard EngineCar_175
engineCar_175 = locationWith
  EngineCar_175
  Cards.engineCar_175
  4
  (PerPlayer 2)
  (connectsToL .~ singleton LeftOf)

instance HasModifiersFor EngineCar_175 where
  getModifiersFor _ target (EngineCar_175 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- field LocationClues leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage EngineCar_175 where
  runMessage msg (EngineCar_175 attrs) = EngineCar_175 <$> runMessage msg attrs
