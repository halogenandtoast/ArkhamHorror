module Arkham.Location.Cards.OfficeSpectral
  ( officeSpectral
  , OfficeSpectral(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype OfficeSpectral = OfficeSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

officeSpectral :: LocationCard OfficeSpectral
officeSpectral = location OfficeSpectral Cards.officeSpectral 4 (PerPlayer 2)

instance HasAbilities OfficeSpectral where
  getAbilities (OfficeSpectral attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage OfficeSpectral where
  runMessage msg (OfficeSpectral attrs) =
    OfficeSpectral <$> runMessage msg attrs
