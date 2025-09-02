module Arkham.Asset.Assets.HeliosTelescopeGateToTheCosmos (heliosTelescopeGateToTheCosmos) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype HeliosTelescopeGateToTheCosmos = HeliosTelescopeGateToTheCosmos AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heliosTelescopeGateToTheCosmos :: AssetCard HeliosTelescopeGateToTheCosmos
heliosTelescopeGateToTheCosmos = asset HeliosTelescopeGateToTheCosmos Cards.heliosTelescopeGateToTheCosmos

instance HasAbilities HeliosTelescopeGateToTheCosmos where
  getAbilities (HeliosTelescopeGateToTheCosmos a) =
    [ controlled a 1 (exists $ at_ YourLocation <> ExhaustedEnemy <> NonEliteEnemy)
        $ actionAbilityWithCost (assetUseCost a Shard 1)
    , playerLimit PerRound $ fastAbility a 2 Free (exists $ colocatedWithMatch You <> not_ You)
    , restricted a 3 (exists $ not_ You)
        $ SilentForcedAbility
        $ InvestigatorDefeated #when ByAny (ControlsAsset $ be a)
    ]

instance RunMessage HeliosTelescopeGateToTheCosmos where
  runMessage msg a@(HeliosTelescopeGateToTheCosmos attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ at_ (locationWithInvestigator iid) <> ExhaustedEnemy <> NonEliteEnemy
      chooseOrRunOneM iid do
        targets enemies $ toDiscardBy iid (attrs.ability 1)
      doStep (attrs.use Shard) msg
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      enemies <- select $ InPlayEnemy EliteEnemy
      chooseOrRunOneM iid $ targets enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
      doStep (n - 1) msg'
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      others <- select $ colocatedWith iid <> not_ (InvestigatorWithId iid)
      chooseOrRunOneM iid $ targets others (`takeControlOfAsset` attrs)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      for_ attrs.controller \controller -> do
        others <- select $ not_ (be controller) <> NearestToLocation (locationWithInvestigator controller)
        chooseOrRunOneM iid $ targets others (`takeControlOfAsset` attrs)
      pure a
    _ -> HeliosTelescopeGateToTheCosmos <$> liftRunMessage msg attrs
