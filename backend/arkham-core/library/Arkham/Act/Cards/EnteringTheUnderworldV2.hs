module Arkham.Act.Cards.EnteringTheUnderworldV2
  ( EnteringTheUnderworldV2(..)
  , enteringTheUnderworldV2
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype EnteringTheUnderworldV2 = EnteringTheUnderworldV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

enteringTheUnderworldV2 :: ActCard EnteringTheUnderworldV2
enteringTheUnderworldV2 = act (1, A) EnteringTheUnderworldV2 Cards.enteringTheUnderworldV2 Nothing

instance RunMessage EnteringTheUnderworldV2 where
  runMessage msg a@(EnteringTheUnderworldV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      pure a
    _ -> EnteringTheUnderworldV2 <$> lift (runMessage msg attrs)
