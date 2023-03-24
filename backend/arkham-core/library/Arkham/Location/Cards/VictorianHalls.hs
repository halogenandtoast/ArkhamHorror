module Arkham.Location.Cards.VictorianHalls
  ( victorianHalls
  , VictorianHalls(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype VictorianHalls = VictorianHalls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victorianHalls :: LocationCard VictorianHalls
victorianHalls = location VictorianHalls Cards.victorianHalls 4 (Static 0)

instance HasAbilities VictorianHalls where
  getAbilities (VictorianHalls attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage VictorianHalls where
  runMessage msg (VictorianHalls attrs) =
    VictorianHalls <$> runMessage msg attrs
