module Arkham.Enemy.Cards.EnragedGug (enragedGug) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher.Asset
import Arkham.Strategy
import Arkham.Trait (Trait (Guest))

newtype EnragedGug = EnragedGug EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

enragedGug :: EnemyCard EnragedGug
enragedGug =
  enemyWith
    EnragedGug
    Cards.enragedGug
    (3, Static 3, 2)
    (1, 1)
    (damageStrategyL .~ HorrorAssetsFirst (AssetWithTrait Guest))

instance RunMessage EnragedGug where
  runMessage msg (EnragedGug attrs) = EnragedGug <$> runMessage msg attrs
