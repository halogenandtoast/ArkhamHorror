module Arkham.Enemy.Cards.TracesToNowhere.CrookedCops (crookedCops) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (PerformAction)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype CrookedCops = CrookedCops EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crookedCops :: EnemyCard CrookedCops
crookedCops =
  enemyWith CrookedCops Cards.crookedCops $ spawnAtL ?~ SpawnAt (connectedFrom YourLocation)

instance HasAbilities CrookedCops where
  getAbilities (CrookedCops a) =
    extend
      a
      [ mkAbility a 1 $ forced $ PerformAction #after Anyone #parley
      , restricted a 2 OnSameLocation $ parleyAction (ResourceCost 2)
      ]

instance RunMessage CrookedCops where
  runMessage msg e@(CrookedCops attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> enemyMoveTo (attrs.ability 1) attrs lid
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      automaticallyEvadeEnemy iid attrs
      pure e
    _ -> CrookedCops <$> liftRunMessage msg attrs
