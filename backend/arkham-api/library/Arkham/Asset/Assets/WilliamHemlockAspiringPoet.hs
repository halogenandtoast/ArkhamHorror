module Arkham.Asset.Assets.WilliamHemlockAspiringPoet (williamHemlockAspiringPoet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (Resident (..), codex, decreaseRelationshipLevel)

newtype WilliamHemlockAspiringPoet = WilliamHemlockAspiringPoet AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamHemlockAspiringPoet :: AssetCard WilliamHemlockAspiringPoet
williamHemlockAspiringPoet = assetWith WilliamHemlockAspiringPoet Cards.williamHemlockAspiringPoet $ (healthL ?~ 2) . (sanityL ?~ 2)

instance HasModifiersFor WilliamHemlockAspiringPoet where
  getModifiersFor (WilliamHemlockAspiringPoet a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities WilliamHemlockAspiringPoet where
  getAbilities (WilliamHemlockAspiringPoet a) =
    [ mkAbility a 99 $ forced $ AssetDefeated #when ByAny (be a)
    ,groupLimit PerGame $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage WilliamHemlockAspiringPoet where
  runMessage msg a@(WilliamHemlockAspiringPoet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) n | n == 1 || n == 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability n) attrs #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 4
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      codex iid (attrs.ability 2) 4
      pure a
    UseCardAbility _ (isSource attrs -> True) 99 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      push $ SetAsideCards [toCard attrs]
      decreaseRelationshipLevel WilliamHemlock 1
      pure a
    _ -> WilliamHemlockAspiringPoet <$> liftRunMessage msg attrs
