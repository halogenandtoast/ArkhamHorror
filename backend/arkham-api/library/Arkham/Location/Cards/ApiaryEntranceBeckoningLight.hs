module Arkham.Location.Cards.ApiaryEntranceBeckoningLight (apiaryEntranceBeckoningLight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ApiaryEntranceBeckoningLight = ApiaryEntranceBeckoningLight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apiaryEntranceBeckoningLight :: LocationCard ApiaryEntranceBeckoningLight
apiaryEntranceBeckoningLight = location ApiaryEntranceBeckoningLight Cards.apiaryEntranceBeckoningLight 1 (Static 1)

-- TODO: abilities

instance RunMessage ApiaryEntranceBeckoningLight where
  runMessage msg (ApiaryEntranceBeckoningLight attrs) = runQueueT $ ApiaryEntranceBeckoningLight <$> liftRunMessage msg attrs
