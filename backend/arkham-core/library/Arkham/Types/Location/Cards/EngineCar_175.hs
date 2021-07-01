module Arkham.Types.Location.Cards.EngineCar_175
  ( engineCar_175
  , EngineCar_175(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (engineCar_175)
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Query

newtype EngineCar_175 = EngineCar_175 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineCar_175 :: LocationId -> EngineCar_175
engineCar_175 =
  EngineCar_175
    . (connectsToL .~ singleton LeftOf)
    . baseAttrs
        Cards.engineCar_175
        4
        (PerPlayer 2)
        NoSymbol
        []

instance HasCount ClueCount env LocationId => HasModifiersFor env EngineCar_175 where
  getModifiersFor _ target (EngineCar_175 location@LocationAttrs {..})
    | isTarget location target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers
          location
          [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env EngineCar_175 where
  getActions iid window (EngineCar_175 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env EngineCar_175 where
  runMessage msg (EngineCar_175 attrs) = EngineCar_175 <$> runMessage msg attrs
