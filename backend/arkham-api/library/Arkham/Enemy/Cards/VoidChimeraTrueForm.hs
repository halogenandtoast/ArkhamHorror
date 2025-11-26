module Arkham.Enemy.Cards.VoidChimeraTrueForm (voidChimeraTrueForm) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype VoidChimeraTrueForm = VoidChimeraTrueForm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voidChimeraTrueForm :: EnemyCard VoidChimeraTrueForm
voidChimeraTrueForm = enemy VoidChimeraTrueForm Cards.voidChimeraTrueForm (0, Static 1, 0) (0, 0)

instance RunMessage VoidChimeraTrueForm where
  runMessage msg (VoidChimeraTrueForm attrs) = runQueueT $ case msg of
    _ -> VoidChimeraTrueForm <$> liftRunMessage msg attrs
