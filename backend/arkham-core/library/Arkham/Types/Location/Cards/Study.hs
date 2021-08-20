module Arkham.Types.Location.Cards.Study where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (study)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype Study = Study LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

study :: LocationCard Study
study = location Study Cards.study 2 (PerPlayer 2) Circle []

instance HasModifiersFor env Study

instance HasAbilities env Study where
  getAbilities i window (Study attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env Study where
  runMessage msg (Study attrs) = Study <$> runMessage msg attrs
