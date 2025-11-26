module Arkham.Act.Cards.LastStand (lastStand) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype LastStand = LastStand ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lastStand :: ActCard LastStand
lastStand = act (4, A) LastStand Cards.lastStand Nothing

instance RunMessage LastStand where
  runMessage msg a@(LastStand attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> LastStand <$> liftRunMessage msg attrs
