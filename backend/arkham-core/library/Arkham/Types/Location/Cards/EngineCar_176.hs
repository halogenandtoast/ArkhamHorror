module Arkham.Types.Location.Cards.EngineCar_176
  ( engineCar_176
  , EngineCar_176(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (engineCar_176)
import Arkham.Types.Card.CardMatcher
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query

newtype EngineCar_176 = EngineCar_176 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineCar_176 :: LocationId -> EngineCar_176
engineCar_176 =
  EngineCar_176
    . (connectsToL .~ singleton LeftOf)
    . baseAttrs
        Cards.engineCar_176
        2
        (PerPlayer 2)
        NoSymbol
        []

instance HasCount ClueCount env LocationId => HasModifiersFor env EngineCar_176 where
  getModifiersFor _ target (EngineCar_176 location@LocationAttrs {..})
    | isTarget location target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers
          location
          [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env EngineCar_176 where
  getActions iid window (EngineCar_176 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env EngineCar_176 where
  runMessage msg (EngineCar_176 attrs) = case msg of
    RevealLocation (Just iid) lid | lid == locationId attrs -> do
      unshiftMessage
        (FindAndDrawEncounterCard iid (CardMatchByCardCode "02182"))
      EngineCar_176 <$> runMessage msg attrs
    _ -> EngineCar_176 <$> runMessage msg attrs
