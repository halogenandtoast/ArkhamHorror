module Arkham.Location.Cards.MasterBedroomSpectral
  ( masterBedroomSpectral
  , MasterBedroomSpectral(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype MasterBedroomSpectral = MasterBedroomSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masterBedroomSpectral :: LocationCard MasterBedroomSpectral
masterBedroomSpectral = location MasterBedroomSpectral Cards.masterBedroomSpectral 3 (PerPlayer 1)

instance HasAbilities MasterBedroomSpectral where
  getAbilities (MasterBedroomSpectral attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage MasterBedroomSpectral where
  runMessage msg (MasterBedroomSpectral attrs) =
    MasterBedroomSpectral <$> runMessage msg attrs
