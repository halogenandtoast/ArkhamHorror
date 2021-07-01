module Arkham.Types.Location.Cards.Cellar where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (cellar)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype Cellar = Cellar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cellar :: LocationId -> Cellar
cellar = Cellar . baseAttrs
  Cards.cellar
  4
  (PerPlayer 2)
  Plus
  [Square]

instance HasModifiersFor env Cellar where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Cellar where
  getActions i window (Cellar attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Cellar where
  runMessage msg a@(Cellar attrs@LocationAttrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> a <$ unshiftMessage
      (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0)
    _ -> Cellar <$> runMessage msg attrs
