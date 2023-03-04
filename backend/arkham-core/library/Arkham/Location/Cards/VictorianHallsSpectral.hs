module Arkham.Location.Cards.VictorianHallsSpectral
  ( victorianHallsSpectral
  , VictorianHallsSpectral(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype VictorianHallsSpectral = VictorianHallsSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victorianHallsSpectral :: LocationCard VictorianHallsSpectral
victorianHallsSpectral = location VictorianHallsSpectral Cards.victorianHallsSpectral 4 (Static 0)

instance HasAbilities VictorianHallsSpectral where
  getAbilities (VictorianHallsSpectral attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage VictorianHallsSpectral where
  runMessage msg (VictorianHallsSpectral attrs) =
    VictorianHallsSpectral <$> runMessage msg attrs
