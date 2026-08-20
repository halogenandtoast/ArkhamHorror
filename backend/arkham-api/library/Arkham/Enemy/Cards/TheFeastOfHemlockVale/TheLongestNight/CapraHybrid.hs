module Arkham.Enemy.Cards.TheFeastOfHemlockVale.TheLongestNight.CapraHybrid (capraHybrid) where

import Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.TheLongestNight qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Matcher
import Arkham.Scenarios.TheFeastOfHemlockVale.TheLongestNight.Helpers (pattern IgnoreDecoys)

newtype CapraHybrid = CapraHybrid EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

capraHybrid :: EnemyCard CapraHybrid
capraHybrid =
  enemy CapraHybrid Cards.capraHybrid
    & setSpawnAt (LocationWithFewestEnemies (LocationWithTitle "Outer Fields") AnyEnemy)

instance HasModifiersFor CapraHybrid where
  getModifiersFor (CapraHybrid a) = modifySelf a [IgnoreDecoys]
