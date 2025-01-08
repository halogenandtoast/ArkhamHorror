module Arkham.Enemy.Cards.MemoryOfAnUnrequitedLove (memoryOfAnUnrequitedLove) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (DiscoverClues, EnemyDefeated)
import Arkham.Helpers.Story
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype MemoryOfAnUnrequitedLove = MemoryOfAnUnrequitedLove EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAnUnrequitedLove :: EnemyCard MemoryOfAnUnrequitedLove
memoryOfAnUnrequitedLove =
  enemyWith MemoryOfAnUnrequitedLove Cards.memoryOfAnUnrequitedLove (4, PerPlayer 3, 3) (1, 1)
    $ preyL
    .~ Prey (ControlsAsset $ assetIs Assets.drAmyKenslerProfessorOfBiology)

instance HasAbilities MemoryOfAnUnrequitedLove where
  getAbilities (MemoryOfAnUnrequitedLove a) =
    extend
      a
      [ mkAbility a 1 $ freeReaction $ DiscoverClues #after You (locationWithEnemy a) (atLeast 1)
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MemoryOfAnUnrequitedLove where
  runMessage msg e@(MemoryOfAnUnrequitedLove attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      nonAttackEnemyDamage (attrs.ability 1) n attrs.id
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.memoryOfAnUnrequitedLove
      pure e
    _ -> MemoryOfAnUnrequitedLove <$> liftRunMessage msg attrs
