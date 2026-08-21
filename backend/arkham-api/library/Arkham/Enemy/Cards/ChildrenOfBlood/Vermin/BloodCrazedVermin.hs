module Arkham.Enemy.Cards.ChildrenOfBlood.Vermin.BloodCrazedVermin (bloodCrazedVermin) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.Vermin qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BloodCrazedVermin = BloodCrazedVermin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bloodCrazedVermin :: EnemyCard BloodCrazedVermin
bloodCrazedVermin = enemy BloodCrazedVermin Cards.bloodCrazedVermin

instance RunMessage BloodCrazedVermin where
  runMessage msg (BloodCrazedVermin attrs) = runQueueT $ case msg of
    _ -> BloodCrazedVermin <$> liftRunMessage msg attrs
