module Arkham.Location.Cards.Church (church) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Church = Church LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

church :: LocationCard Church
church = locationWith Church Cards.church 4 (PerPlayer 1) connectsToAdjacent

instance RunMessage Church where
  runMessage msg (Church attrs) = Church <$> runMessage msg attrs
