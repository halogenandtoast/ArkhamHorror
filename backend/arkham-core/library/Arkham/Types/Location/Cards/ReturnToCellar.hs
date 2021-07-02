module Arkham.Types.Location.Cards.ReturnToCellar (returnToCellar, ReturnToCellar(..)) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (returnToCellar)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype ReturnToCellar = ReturnToCellar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCellar :: LocationId -> ReturnToCellar
returnToCellar = ReturnToCellar . baseAttrs
  Cards.returnToCellar
  2
  (PerPlayer 1)
  Plus
  [Square, Squiggle]

instance HasModifiersFor env ReturnToCellar where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ReturnToCellar where
  getActions i window (ReturnToCellar attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ReturnToCellar where
  runMessage msg (ReturnToCellar attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      deepBelowYourHouseId <- getRandom
      unshiftMessage (PlaceLocation "50021" deepBelowYourHouseId)
      ReturnToCellar <$> runMessage msg attrs
    _ -> ReturnToCellar <$> runMessage msg attrs
