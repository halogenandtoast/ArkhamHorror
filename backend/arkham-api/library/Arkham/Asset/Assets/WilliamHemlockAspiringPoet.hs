module Arkham.Asset.Assets.WilliamHemlockAspiringPoet (williamHemlockAspiringPoet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher

newtype WilliamHemlockAspiringPoet = WilliamHemlockAspiringPoet AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamHemlockAspiringPoet :: AssetCard WilliamHemlockAspiringPoet
williamHemlockAspiringPoet =
  assetWith WilliamHemlockAspiringPoet Cards.williamHemlockAspiringPoet
    $ (healthL ?~ 2)
    . (sanityL ?~ 2)

instance HasModifiersFor WilliamHemlockAspiringPoet where
  getModifiersFor (WilliamHemlockAspiringPoet a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities WilliamHemlockAspiringPoet where
  getAbilities (WilliamHemlockAspiringPoet a) =
    [ skillTestAbility $ restricted a 1 (OnSameLocation <> youCanTriggerCodex 4) parleyAction_
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage WilliamHemlockAspiringPoet where
  runMessage msg a@(WilliamHemlockAspiringPoet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 4
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      setCardAside attrs
      decreaseRelationshipLevel WilliamHemlock 1
      pure a
    _ -> WilliamHemlockAspiringPoet <$> liftRunMessage msg attrs
