module Arkham.Types.Location.Cards.ReturnToCellar
  ( returnToCellar
  , ReturnToCellar(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype ReturnToCellar = ReturnToCellar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCellar :: LocationCard ReturnToCellar
returnToCellar = location
  ReturnToCellar
  Cards.returnToCellar
  2
  (PerPlayer 1)
  Plus
  [Square, Squiggle]

instance HasModifiersFor env ReturnToCellar

instance ActionRunner env => HasAbilities env ReturnToCellar where
  getAbilities i window (ReturnToCellar attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env ReturnToCellar where
  runMessage msg (ReturnToCellar attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      deepBelowYourHouseId <- getRandom
      push (PlaceLocation deepBelowYourHouseId Cards.deepBelowYourHouse)
      ReturnToCellar <$> runMessage msg attrs
    _ -> ReturnToCellar <$> runMessage msg attrs
