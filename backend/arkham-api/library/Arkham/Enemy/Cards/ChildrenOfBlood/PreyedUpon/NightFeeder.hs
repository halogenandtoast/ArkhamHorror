module Arkham.Enemy.Cards.ChildrenOfBlood.PreyedUpon.NightFeeder (nightFeeder) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.PreyedUpon qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype NightFeeder = NightFeeder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nightFeeder :: EnemyCard NightFeeder
nightFeeder = enemy NightFeeder Cards.nightFeeder

instance RunMessage NightFeeder where
  runMessage msg (NightFeeder attrs) = runQueueT $ case msg of
    _ -> NightFeeder <$> liftRunMessage msg attrs
