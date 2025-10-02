module Arkham.Act.Cards.CaughtRedHanded (caughtRedHanded) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype CaughtRedHanded = CaughtRedHanded ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

caughtRedHanded :: ActCard CaughtRedHanded
caughtRedHanded = act (4, A) CaughtRedHanded Cards.caughtRedHanded Nothing

instance RunMessage CaughtRedHanded where
  runMessage msg a@(CaughtRedHanded attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> CaughtRedHanded <$> liftRunMessage msg attrs
