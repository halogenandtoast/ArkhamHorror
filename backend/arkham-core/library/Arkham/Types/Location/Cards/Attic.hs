module Arkham.Types.Location.Cards.Attic where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype Attic = Attic LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attic :: Attic
attic = Attic $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "01113"
    (Name "Attic" Nothing)
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
