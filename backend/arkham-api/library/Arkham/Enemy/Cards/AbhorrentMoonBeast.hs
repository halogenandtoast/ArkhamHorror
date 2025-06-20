module Arkham.Enemy.Cards.AbhorrentMoonBeast (abhorrentMoonBeast) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher.Asset
import Arkham.Strategy
import Arkham.Trait (Trait (Guest))

newtype AbhorrentMoonBeast = AbhorrentMoonBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

abhorrentMoonBeast :: EnemyCard AbhorrentMoonBeast
abhorrentMoonBeast =
  enemyWith
    AbhorrentMoonBeast
    Cards.abhorrentMoonBeast
    (2, Static 3, 4)
    (1, 1)
    (damageStrategyL .~ DamageAssetsFirst (AssetWithTrait Guest))

instance RunMessage AbhorrentMoonBeast where
  runMessage msg (AbhorrentMoonBeast attrs) =
    AbhorrentMoonBeast <$> runMessage msg attrs
