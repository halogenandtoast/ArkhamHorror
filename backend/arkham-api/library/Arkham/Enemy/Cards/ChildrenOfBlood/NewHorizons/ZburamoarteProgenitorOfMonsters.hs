module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.ZburamoarteProgenitorOfMonsters (zburamoarteProgenitorOfMonsters) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher

newtype ZburamoarteProgenitorOfMonsters = ZburamoarteProgenitorOfMonsters EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zburamoarteProgenitorOfMonsters :: EnemyCard ZburamoarteProgenitorOfMonsters
zburamoarteProgenitorOfMonsters = enemy ZburamoarteProgenitorOfMonsters Cards.zburamoarteProgenitorOfMonsters

instance HasModifiersFor ZburamoarteProgenitorOfMonsters where
  getModifiersFor (ZburamoarteProgenitorOfMonsters a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier (3 * n), CannotMove]

instance HasAbilities ZburamoarteProgenitorOfMonsters where
  getAbilities (ZburamoarteProgenitorOfMonsters a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDealsDamage #after (be a)

instance RunMessage ZburamoarteProgenitorOfMonsters where
  runMessage msg e@(ZburamoarteProgenitorOfMonsters attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 4
      pure e
    _ -> ZburamoarteProgenitorOfMonsters <$> liftRunMessage msg attrs
