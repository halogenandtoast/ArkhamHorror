module Arkham.Enemy.Cards.MemoryOfATerribleDiscovery (memoryOfATerribleDiscovery) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks, EnemyDefeated)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype MemoryOfATerribleDiscovery = MemoryOfATerribleDiscovery EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfATerribleDiscovery :: EnemyCard MemoryOfATerribleDiscovery
memoryOfATerribleDiscovery =
  enemyWith MemoryOfATerribleDiscovery Cards.memoryOfATerribleDiscovery (3, PerPlayer 4, 3) (1, 1)
    $ preyL
    .~ Prey (ControlsAsset $ assetIs Assets.averyClaypoolAntarcticGuide)

instance HasAbilities MemoryOfATerribleDiscovery where
  getAbilities (MemoryOfATerribleDiscovery a) =
    extend
      a
      [ restricted a 1 OnSameLocation $ parleyAction (AddFrostTokenCost 1)
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MemoryOfATerribleDiscovery where
  runMessage msg e@(MemoryOfATerribleDiscovery attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage (attrs.ability 1) 4 attrs.id
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.memoryOfATerribleDiscovery
      pure e
    _ -> MemoryOfATerribleDiscovery <$> liftRunMessage msg attrs
