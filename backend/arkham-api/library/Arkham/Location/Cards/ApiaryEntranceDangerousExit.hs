module Arkham.Location.Cards.ApiaryEntranceDangerousExit (apiaryEntranceDangerousExit) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ApiaryEntranceDangerousExit = ApiaryEntranceDangerousExit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apiaryEntranceDangerousExit :: LocationCard ApiaryEntranceDangerousExit
apiaryEntranceDangerousExit = location ApiaryEntranceDangerousExit Cards.apiaryEntranceDangerousExit 4 (Static 0)

-- TODO: abilities

instance RunMessage ApiaryEntranceDangerousExit where
  runMessage msg (ApiaryEntranceDangerousExit attrs) = runQueueT $ ApiaryEntranceDangerousExit <$> liftRunMessage msg attrs
