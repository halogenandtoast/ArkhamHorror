module Arkham.Enemy.Cards.SlitheringHybrid (slitheringHybrid) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Matcher
import Arkham.Scenarios.TheLongestNight.Helpers (
  pattern IgnoreBarriers,
  pattern IgnoreDecoys,
  pattern IgnoreFireDamage,
  pattern IgnoreTraps,
 )

newtype SlitheringHybrid = SlitheringHybrid EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slitheringHybrid :: EnemyCard SlitheringHybrid
slitheringHybrid =
  enemy SlitheringHybrid Cards.slitheringHybrid
    & setSpawnAt (LocationWithMostEnemies (LocationWithTitle "Outer Fields") AnyEnemy)

instance HasModifiersFor SlitheringHybrid where
  getModifiersFor (SlitheringHybrid a) =
    modifySelf a [IgnoreBarriers, IgnoreDecoys, IgnoreTraps, IgnoreFireDamage]
