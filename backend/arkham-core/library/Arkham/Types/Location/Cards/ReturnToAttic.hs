module Arkham.Types.Location.Cards.ReturnToAttic where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype ReturnToAttic = ReturnToAttic LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToAttic :: LocationId -> ReturnToAttic
returnToAttic =
  ReturnToAttic
    . baseAttrs
        "50018"
        "Attic"
        EncounterSet.ReturnToTheGathering
        3
        (PerPlayer 1)
        Triangle
        [Square, Moon]
        []

instance HasModifiersFor env ReturnToAttic where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ReturnToAttic where
  getActions i window (ReturnToAttic attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ReturnToAttic where
  runMessage msg (ReturnToAttic attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      farAboveYourHouseId <- getRandom
      unshiftMessage (PlaceLocation "50019" farAboveYourHouseId)
      ReturnToAttic <$> runMessage msg attrs
    _ -> ReturnToAttic <$> runMessage msg attrs
