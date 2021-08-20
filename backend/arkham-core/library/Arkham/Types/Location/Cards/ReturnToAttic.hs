module Arkham.Types.Location.Cards.ReturnToAttic
  ( returnToAttic
  , ReturnToAttic(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype ReturnToAttic = ReturnToAttic LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToAttic :: LocationCard ReturnToAttic
returnToAttic = location
  ReturnToAttic
  Cards.returnToAttic
  3
  (PerPlayer 1)
  Triangle
  [Square, Moon]

instance HasModifiersFor env ReturnToAttic

instance HasAbilities env ReturnToAttic where
  getAbilities i window (ReturnToAttic attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env ReturnToAttic where
  runMessage msg (ReturnToAttic attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      farAboveYourHouseId <- getRandom
      push (PlaceLocation farAboveYourHouseId Cards.farAboveYourHouse)
      ReturnToAttic <$> runMessage msg attrs
    _ -> ReturnToAttic <$> runMessage msg attrs
