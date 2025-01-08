module Arkham.Enemy.Cards.MemoryOfAMissingFather (memoryOfAMissingFather) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype MemoryOfAMissingFather = MemoryOfAMissingFather EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAMissingFather :: EnemyCard MemoryOfAMissingFather
memoryOfAMissingFather =
  enemyWith MemoryOfAMissingFather Cards.memoryOfAMissingFather (3, PerPlayer 4, 4) (1, 1)
    $ preyL
    .~ Prey (ControlsAsset $ assetIs Assets.takadaHirokoAeroplaneMechanic)

instance HasAbilities MemoryOfAMissingFather where
  getAbilities (MemoryOfAMissingFather a) =
    extend
      a
      [ restricted a 1 (thisExists a ExhaustedEnemy)
          $ freeReaction
          $ GainsResources #after You AnySource AnyValue
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MemoryOfAMissingFather where
  runMessage msg e@(MemoryOfAMissingFather attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage (attrs.ability 1) 1 attrs.id
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.memoryOfAMissingFather
      pure e
    _ -> MemoryOfAMissingFather <$> liftRunMessage msg attrs
