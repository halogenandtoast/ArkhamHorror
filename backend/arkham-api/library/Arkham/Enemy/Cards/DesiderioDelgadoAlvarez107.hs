module Arkham.Enemy.Cards.DesiderioDelgadoAlvarez107 (desiderioDelgadoAlvarez107) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DesiderioDelgadoAlvarez107 = DesiderioDelgadoAlvarez107 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

desiderioDelgadoAlvarez107 :: EnemyCard DesiderioDelgadoAlvarez107
desiderioDelgadoAlvarez107 = enemy DesiderioDelgadoAlvarez107 Cards.desiderioDelgadoAlvarez107 (4, Static 2, 3) (2, 1)

instance RunMessage DesiderioDelgadoAlvarez107 where
  runMessage msg (DesiderioDelgadoAlvarez107 attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      continue iid $ do_ msg
      pure $ DesiderioDelgadoAlvarez107 $ attrs & flippedL .~ True
    Do (LookAtRevealed _iid _ (isTarget attrs -> True)) -> do
      pure $ DesiderioDelgadoAlvarez107 $ attrs & flippedL .~ False
    _ -> DesiderioDelgadoAlvarez107 <$> liftRunMessage msg attrs
