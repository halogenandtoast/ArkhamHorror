module Arkham.Location.Cards.HazuthKleg
  ( hazuthKleg
  , HazuthKleg(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HazuthKleg = HazuthKleg LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hazuthKleg :: LocationCard HazuthKleg
hazuthKleg = location HazuthKleg Cards.hazuthKleg 4 (PerPlayer 1)

instance HasAbilities HazuthKleg where
  getAbilities (HazuthKleg attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage HazuthKleg where
  runMessage msg (HazuthKleg attrs) =
    HazuthKleg <$> runMessage msg attrs
