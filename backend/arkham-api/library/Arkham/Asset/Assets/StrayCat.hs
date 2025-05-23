module Arkham.Asset.Assets.StrayCat (strayCat) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype StrayCat = StrayCat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetCard StrayCat
strayCat = ally StrayCat Cards.strayCat (1, 0)

instance HasAbilities StrayCat where
  getAbilities (StrayCat a) =
    [ controlled a 1 (exists (at_ YourLocation <> NonEliteEnemy <> EnemyWithoutModifier CannotBeEvaded))
        $ FastAbility
        $ discardCost a
    ]

instance RunMessage StrayCat where
  runMessage msg a@(StrayCat attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyAtLocationWith iid <> NonEliteEnemy <> EnemyWithoutModifier CannotBeEvaded
      chooseTargetM iid enemies (automaticallyEvadeEnemy iid)
      pure a
    _ -> StrayCat <$> liftRunMessage msg attrs
