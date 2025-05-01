module Arkham.Asset.Assets.ZoeysCross (zoeysCross) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype ZoeysCross = ZoeysCross AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeysCross :: AssetCard ZoeysCross
zoeysCross = asset ZoeysCross Cards.zoeysCross

instance HasAbilities ZoeysCross where
  getAbilities (ZoeysCross x) =
    [ controlled x 1 CanDealDamage
        $ triggered (EnemyEngaged #after You (canBeDamagedBy x)) (exhaust x <> ResourceCost 1)
    ]

instance RunMessage ZoeysCross where
  runMessage msg a@(ZoeysCross attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (engagedEnemy -> eid) _ -> do
      nonAttackEnemyDamage (Just iid) attrs 1 eid
      pure a
    _ -> ZoeysCross <$> liftRunMessage msg attrs
