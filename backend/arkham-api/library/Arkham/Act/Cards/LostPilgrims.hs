module Arkham.Act.Cards.LostPilgrims (lostPilgrims) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype LostPilgrims = LostPilgrims ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostPilgrims :: ActCard LostPilgrims
lostPilgrims = act (2, A) LostPilgrims Cards.lostPilgrims Nothing

-- TODO: abilities
instance HasAbilities LostPilgrims where
  getAbilities _ = []

instance RunMessage LostPilgrims where
  runMessage msg (LostPilgrims attrs) = runQueueT $ LostPilgrims <$> liftRunMessage msg attrs
