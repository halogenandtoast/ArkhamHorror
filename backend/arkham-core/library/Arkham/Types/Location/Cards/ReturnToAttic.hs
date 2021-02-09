module Arkham.Types.Location.Cards.ReturnToAttic where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype ReturnToAttic = ReturnToAttic LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToAttic :: ReturnToAttic
returnToAttic = ReturnToAttic $ baseAttrs
  "50018"
  (Name "Attic" Nothing)
  EncounterSet.ReturnToTheGathering
  3
  (PerPlayer 1)
  Triangle
  [Square, Moon]
  mempty

instance HasModifiersFor env ReturnToAttic where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ReturnToAttic where
  getActions i window (ReturnToAttic attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ReturnToAttic where
  runMessage msg (ReturnToAttic attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessage (PlaceLocation "50019")
      ReturnToAttic <$> runMessage msg attrs
    _ -> ReturnToAttic <$> runMessage msg attrs
