module Arkham.Types.Location.Cards.EngineCar_175
  ( engineCar_175
  , EngineCar_175(..)
  )
where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype EngineCar_175 = EngineCar_175 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineCar_175 :: EngineCar_175
engineCar_175 = EngineCar_175
  $ base { locationVictory = Just 1, locationConnectsTo = singleton LeftOf }
 where
  base = baseAttrs
    "02175"
    (Name "Engine Car" Nothing)
    EncounterSet.TheEssexCountyExpress
    4
    (PerPlayer 2)
    NoSymbol
    []
    (singleton Train)

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
