{- | The runner behind a debug-authored custom enemy.

It adds no abilities of its own: everything it does comes from the printed def
(fight/health/evade, damage, keywords, traits, victory), which is exactly what
"behaves according to its def" means for a vanilla enemy.
-}
module Arkham.Custom.Enemy (CustomEnemy (..), customEnemy) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Enemy.Import.Lifted

newtype CustomEnemy = CustomEnemy EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

customEnemy :: CardDef -> EnemyCard CustomEnemy
customEnemy = enemy CustomEnemy

instance RunMessage CustomEnemy where
  runMessage msg (CustomEnemy attrs) = CustomEnemy <$> runMessage msg attrs
