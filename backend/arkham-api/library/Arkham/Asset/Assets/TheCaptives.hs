module Arkham.Asset.Assets.TheCaptives (theCaptives) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message (pattern R3)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection

newtype TheCaptives = TheCaptives AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCaptives :: AssetCard TheCaptives
theCaptives = assetWith TheCaptives Cards.theCaptives (healthL ?~ 10)

instance HasAbilities TheCaptives where
  getAbilities (TheCaptives a) =
    [mkAbility a 1 $ forced $ AssetDefeated #when ByAny (be a)]

instance RunMessage TheCaptives where
  runMessage msg a@(TheCaptives attrs) = runQueueT $ case msg of
    EnemiesAttack -> do
      withLocationOf attrs \loc -> do
        selectEach (#ready <> #unengaged <> enemyAt loc <> EnemyWithoutModifier CannotAttack) \enemy -> do
          healthDmg <- field EnemyHealthDamage enemy
          sanityDmg <- field EnemySanityDamage enemy
          dealAssetDamage attrs.id (EnemyAttackSource enemy) (healthDmg + sanityDmg)
          exhaustEnemy attrs enemy
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R3
      pure a
    _ -> TheCaptives <$> liftRunMessage msg attrs
