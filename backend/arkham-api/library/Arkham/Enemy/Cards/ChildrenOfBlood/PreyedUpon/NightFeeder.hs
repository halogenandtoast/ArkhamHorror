module Arkham.Enemy.Cards.ChildrenOfBlood.PreyedUpon.NightFeeder (nightFeeder) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.PreyedUpon qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype NightFeeder = NightFeeder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightFeeder :: EnemyCard NightFeeder
nightFeeder = enemy NightFeeder Cards.nightFeeder

instance HasAbilities NightFeeder where
  getAbilities (NightFeeder a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDealsDamage #after (be a)

instance RunMessage NightFeeder where
  runMessage msg e@(NightFeeder attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> NightFeeder <$> liftRunMessage msg attrs
