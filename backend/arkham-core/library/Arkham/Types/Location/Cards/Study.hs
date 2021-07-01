module Arkham.Types.Location.Cards.Study where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (study)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol

newtype Study = Study LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

study :: LocationId -> Study
study = Study . baseAttrs
  Cards.study
  2
  (PerPlayer 2)
  Circle
  []

instance HasModifiersFor env Study where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Study where
  getActions i window (Study attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Study where
  runMessage msg (Study attrs) = Study <$> runMessage msg attrs
