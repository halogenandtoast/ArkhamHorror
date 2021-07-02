module Arkham.Types.Location.Cards.HumanitiesBuilding where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (humanitiesBuilding)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query

newtype HumanitiesBuilding = HumanitiesBuilding LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

humanitiesBuilding :: LocationId -> HumanitiesBuilding
humanitiesBuilding = HumanitiesBuilding . baseAttrs
  Cards.humanitiesBuilding
  3
  (PerPlayer 2)
  Square
  [Plus, Triangle]

instance HasModifiersFor env HumanitiesBuilding where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HumanitiesBuilding where
  getActions i window (HumanitiesBuilding attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env HumanitiesBuilding where
  runMessage msg l@(HumanitiesBuilding attrs) = case msg of
    EndTurn iid | iid `elem` locationInvestigators attrs -> do
      horror <- unHorrorCount <$> getCount iid
      l <$ when
        (horror > 0)
        (unshiftMessage $ DiscardTopOfDeck iid horror Nothing)
    _ -> HumanitiesBuilding <$> runMessage msg attrs
