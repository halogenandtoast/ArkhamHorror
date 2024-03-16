module Arkham.Act.Cards.EnteringTheUnderworldV1
  ( EnteringTheUnderworldV1(..)
  , enteringTheUnderworldV1
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype EnteringTheUnderworldV1 = EnteringTheUnderworldV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

enteringTheUnderworldV1 :: ActCard EnteringTheUnderworldV1
enteringTheUnderworldV1 = act (1, A) EnteringTheUnderworldV1 Cards.enteringTheUnderworldV1 Nothing

instance RunMessage EnteringTheUnderworldV1 where
  runMessage msg a@(EnteringTheUnderworldV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      pure a
    _ -> EnteringTheUnderworldV1 <$> lift (runMessage msg attrs)
