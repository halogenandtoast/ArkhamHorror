module Arkham.Asset.Assets.ShardOfYchlechtOtherworldlyRemnant (shardOfYchlecht) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Window (enteringEnemy, getAttackDetails)
import Arkham.Matcher
import Arkham.Trait (Trait (DeepOne, StarSpawn))

newtype ShardOfYchlechtOtherworldlyRemnant = ShardOfYchlechtOtherworldlyRemnant AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shardOfYchlecht :: AssetCard ShardOfYchlechtOtherworldlyRemnant
shardOfYchlecht = asset ShardOfYchlechtOtherworldlyRemnant Cards.shardOfYchlecht

starSpawnOrDeepOne :: EnemyMatcher
starSpawnOrDeepOne = mapOneOf EnemyWithTrait [StarSpawn, DeepOne]

instance HasAbilities ShardOfYchlechtOtherworldlyRemnant where
  getAbilities (ShardOfYchlechtOtherworldlyRemnant a) =
    [ limited (MaxPer Cards.shardOfYchlecht PerRound 1)
        $ controlled_ a 1
        $ freeReaction
          (EnemyAttacks #when You (CancelableEnemyAttack AnyEnemyAttack) starSpawnOrDeepOne)
    , controlled_ a 2
        $ triggered (EnemyEntersYourLocation #when starSpawnOrDeepOne) (exhaust a)
    ]

instance RunMessage ShardOfYchlechtOtherworldlyRemnant where
  runMessage msg a@(ShardOfYchlechtOtherworldlyRemnant attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getAttackDetails -> details) _ -> do
      cancelAttack attrs details
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 (enteringEnemy -> eid) _ -> do
      exhaustThis eid
      pure a
    _ -> ShardOfYchlechtOtherworldlyRemnant <$> liftRunMessage msg attrs
