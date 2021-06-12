module Arkham.Types.Location.Cards.Attic where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source

newtype Attic = Attic LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attic :: LocationId -> Attic
attic =
  Attic
    . (victoryL ?~ 1)
    . baseAttrs
        "01113"
        "Attic"
        EncounterSet.TheGathering
        1
        (PerPlayer 2)
        Triangle
        [Square]
        []

instance HasModifiersFor env Attic where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Attic where
  getActions i window (Attic attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Attic where
  runMessage msg a@(Attic attrs@LocationAttrs { locationId }) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> a <$ unshiftMessage
      (InvestigatorAssignDamage iid (LocationSource locationId) DamageAny 0 1)
    _ -> Attic <$> runMessage msg attrs
