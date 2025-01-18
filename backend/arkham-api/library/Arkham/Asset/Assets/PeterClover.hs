module Arkham.Asset.Assets.PeterClover (peterClover) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message (pattern DealAssetDamage)
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype PeterClover = PeterClover AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterClover :: AssetCard PeterClover
peterClover = allyWith PeterClover Cards.peterClover (3, 2) noSlots

instance HasAbilities PeterClover where
  getAbilities (PeterClover x) =
    [ restricted x 1 Uncontrolled $ forced $ PhaseBegins #when #enemy
    , controlled x 2 (exists (at_ YourLocation <> EnemyWithTrait Criminal)) (FastAbility $ exhaust x)
    ]

instance RunMessage PeterClover where
  runMessage msg a@(PeterClover attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ DealAssetDamage attrs.id (attrs.ability 1) 1 0
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      criminals <- select $ EnemyWithTrait Criminal <> at_ YourLocation
      chooseTargetM iid criminals (automaticallyEvadeEnemy iid)
      pure a
    _ -> PeterClover <$> liftRunMessage msg attrs
