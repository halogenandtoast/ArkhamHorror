module Arkham.Location.Cards.MasterBedroom
  ( masterBedroom
  , MasterBedroom(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype MasterBedroom = MasterBedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masterBedroom :: LocationCard MasterBedroom
masterBedroom = location MasterBedroom Cards.masterBedroom 3 (PerPlayer 1)

instance HasAbilities MasterBedroom where
  getAbilities (MasterBedroom attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage MasterBedroom where
  runMessage msg (MasterBedroom attrs) =
    MasterBedroom <$> runMessage msg attrs
