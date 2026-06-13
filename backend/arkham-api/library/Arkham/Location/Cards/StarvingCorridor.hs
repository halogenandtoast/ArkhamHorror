module Arkham.Location.Cards.StarvingCorridor (starvingCorridor) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype StarvingCorridor = StarvingCorridor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starvingCorridor :: LocationCard StarvingCorridor
starvingCorridor = location StarvingCorridor Cards.starvingCorridor 3 (Static 2)

-- TODO: abilities

instance RunMessage StarvingCorridor where
  runMessage msg (StarvingCorridor attrs) = runQueueT $ StarvingCorridor <$> liftRunMessage msg attrs
