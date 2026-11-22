module Arkham.Asset.Assets.ShardOfYchlechtOtherworldlyRemnant (shardOfYchlecht) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks)
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Helpers.Window (enteringEnemy, getAttackDetails)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Trait (Trait (DeepOne, StarSpawn))

newtype ShardOfYchlechtOtherworldlyRemnant = ShardOfYchlechtOtherworldlyRemnant AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shardOfYchlecht :: AssetCard ShardOfYchlechtOtherworldlyRemnant
shardOfYchlecht = asset ShardOfYchlechtOtherworldlyRemnant Cards.shardOfYchlecht

instance HasModifiersFor ShardOfYchlechtOtherworldlyRemnant where
  getModifiersFor (ShardOfYchlechtOtherworldlyRemnant a) = artifactModifiers a

starSpawnOrDeepOne :: EnemyMatcher
starSpawnOrDeepOne = mapOneOf EnemyWithTrait [StarSpawn, DeepOne]

instance HasAbilities ShardOfYchlechtOtherworldlyRemnant where
  getAbilities (ShardOfYchlechtOtherworldlyRemnant a) =
    [ cardI18n (withI18nTooltip "shardOfYchlecht.ignoreAttack")
        $ limited (MaxPer Cards.shardOfYchlecht PerRound 1)
        $ controlled_ a 1
        $ freeReaction
          (EnemyAttacks #when You (CancelableEnemyAttack AnyEnemyAttack) starSpawnOrDeepOne)
    , -- Only usable once the glyphs naming this ability have been translated.
      cardI18n (withI18nTooltip "shardOfYchlecht.exhaustEnemy")
        $ controlled a 2 (glyphsAllKnown "BXOK")
        $ triggered (EnemyEntersYourLocation #when starSpawnOrDeepOne) (exhaust a)
    , artifactAbility a 3
    ]

instance RunMessage ShardOfYchlechtOtherworldlyRemnant where
  runMessage msg a@(ShardOfYchlechtOtherworldlyRemnant attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getAttackDetails -> details) _ -> do
      cancelAttack attrs details
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 (enteringEnemy -> eid) _ -> do
      exhaustThis eid
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      handOffArtifact iid attrs
      pure a
    _ -> ShardOfYchlechtOtherworldlyRemnant <$> liftRunMessage msg attrs
