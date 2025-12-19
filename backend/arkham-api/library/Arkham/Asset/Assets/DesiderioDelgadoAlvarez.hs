module Arkham.Asset.Assets.DesiderioDelgadoAlvarez (desiderioDelgadoAlvarez) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher

newtype DesiderioDelgadoAlvarez = DesiderioDelgadoAlvarez AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desiderioDelgadoAlvarez :: AssetCard DesiderioDelgadoAlvarez
desiderioDelgadoAlvarez = allyWith DesiderioDelgadoAlvarez Cards.desiderioDelgadoAlvarez (4, 1) noSlots

instance HasAbilities DesiderioDelgadoAlvarez where
  getAbilities (DesiderioDelgadoAlvarez a) =
    [ controlled a 1 CanDealDamage
        $ triggered
          (AssetDealtDamage #when (SourceIsEnemyAttack AnyEnemy) (be a))
          (exhaust a)
    ]

instance RunMessage DesiderioDelgadoAlvarez where
  runMessage msg a@(DesiderioDelgadoAlvarez attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDamageSourceEnemy -> eid) _ -> do
      push $ CancelAssetDamage attrs.id (toSource eid) 1
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      pure a
    _ -> DesiderioDelgadoAlvarez <$> liftRunMessage msg attrs
