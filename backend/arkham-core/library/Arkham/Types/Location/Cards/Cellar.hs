module Arkham.Types.Location.Cards.Cellar where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype Cellar = Cellar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cellar :: Cellar
cellar = Cellar $ (baseAttrs
                    "01114"
                    (Name "Cellar" Nothing)
                    EncounterSet.TheGathering
                    4
                    (PerPlayer 2)
                    Plus
                    [Square]
                    []
                  )
  { locationVictory = Just 1
  }

instance HasModifiersFor env Cellar where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Cellar where
  getActions i window (Cellar attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Cellar where
  runMessage msg a@(Cellar attrs@LocationAttrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId ->
      a <$ unshiftMessage (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0)
    _ -> Cellar <$> runMessage msg attrs
