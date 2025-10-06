module Arkham.Asset.Assets.StrayCat (strayCat) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype StrayCat = StrayCat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetCard StrayCat
strayCat = ally StrayCat Cards.strayCat (1, 0)

instance HasAbilities StrayCat where
  getAbilities (StrayCat a) =
    [ controlled a 1 (canEvadeEnemyAtMatch (a.ability 1) YourLocation NonEliteEnemy)
        $ FastAbility
        $ discardCost a
    ]

instance RunMessage StrayCat where
  runMessage msg a@(StrayCat attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAutomaticallyEvade iid (attrs.ability 1) NonEliteEnemy
      pure a
    _ -> StrayCat <$> liftRunMessage msg attrs
