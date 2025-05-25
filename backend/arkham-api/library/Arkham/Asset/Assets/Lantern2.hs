module Arkham.Asset.Assets.Lantern2 (lantern2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement

newtype Lantern2 = Lantern2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lantern2 :: AssetCard Lantern2
lantern2 = asset Lantern2 Cards.lantern2

instance HasAbilities Lantern2 where
  getAbilities (Lantern2 x) =
    [ investigateAbility x 1 mempty ControlsThis
    , noAOO
        $ controlled x 2 (exists $ EnemyAt YourLocation)
        $ actionAbilityWithCost (OrCost [discardCost x, removeCost x])
    ]

instance RunMessage Lantern2 where
  runMessage msg a@(Lantern2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      withLocationOf iid \lid -> skillTestModifier sid source lid (ShroudModifier (-1))
      investigate sid iid source
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      let
        n =
          case assetPlacement attrs of
            OutOfPlay RemovedZone -> 2
            _ -> 1
      enemies <- select $ enemyAtLocationWith iid
      chooseTargetM iid enemies (nonAttackEnemyDamage (Just iid) source n)
      pure a
    _ -> Lantern2 <$> liftRunMessage msg attrs
