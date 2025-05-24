module Arkham.Asset.Assets.Lantern (lantern) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Lantern = Lantern AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lantern :: AssetCard Lantern
lantern = asset Lantern Cards.lantern

instance HasAbilities Lantern where
  getAbilities (Lantern x) =
    [ investigateAbility x 1 mempty ControlsThis
    , noAOO
        $ controlled x 2 (CanDealDamage <> exists (at_ YourLocation <> canBeDamagedBy (x.ability 2)))
        $ actionAbilityWithCost (discardCost x)
    ]

instance RunMessage Lantern where
  runMessage msg a@(Lantern attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        sid <- getRandom
        skillTestModifier sid (attrs.ability 1) lid (ShroudModifier (-1))
        investigate sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> canBeDamagedBy (attrs.ability 2)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1
      pure a
    _ -> Lantern <$> liftRunMessage msg attrs
