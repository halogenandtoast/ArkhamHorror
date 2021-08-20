module Arkham.Types.Location.Cards.EngineCar_176
  ( engineCar_176
  , EngineCar_176(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (engineCar_176)
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query

newtype EngineCar_176 = EngineCar_176 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineCar_176 :: LocationCard EngineCar_176
engineCar_176 = locationWith
  EngineCar_176
  Cards.engineCar_176
  2
  (PerPlayer 2)
  NoSymbol
  []
  (connectsToL .~ singleton LeftOf)

instance HasCount ClueCount env LocationId => HasModifiersFor env EngineCar_176 where
  getModifiersFor _ target (EngineCar_176 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities env EngineCar_176 where
  getAbilities iid window (EngineCar_176 attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env EngineCar_176 where
  runMessage msg (EngineCar_176 attrs) = case msg of
    RevealLocation (Just iid) lid | lid == locationId attrs -> do
      push (FindAndDrawEncounterCard iid (CardWithCardCode "02182"))
      EngineCar_176 <$> runMessage msg attrs
    _ -> EngineCar_176 <$> runMessage msg attrs
