module Arkham.Asset.Assets.DesiderioDelgadoAlvarez (desiderioDelgadoAlvarez) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Placement

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
    CardEnteredPlay _ card | card.id == attrs.cardId -> do
      keysFor attrs >>= traverse_ (`createScarletKeyAt_` AttachedToAsset attrs.id Nothing)
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (getDamageSourceEnemy -> eid) _ -> do
      push $ CancelAssetDamage attrs.id (toSource eid) 1
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      pure a
    _ -> DesiderioDelgadoAlvarez <$> liftRunMessage msg attrs
