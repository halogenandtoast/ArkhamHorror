module Arkham.Location.Cards.BalconySpectral
  ( balconySpectral
  , BalconySpectral(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype BalconySpectral = BalconySpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balconySpectral :: LocationCard BalconySpectral
balconySpectral = location BalconySpectral Cards.balconySpectral 1 (PerPlayer 1)

instance HasAbilities BalconySpectral where
  getAbilities (BalconySpectral attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BalconySpectral where
  runMessage msg (BalconySpectral attrs) =
    BalconySpectral <$> runMessage msg attrs
