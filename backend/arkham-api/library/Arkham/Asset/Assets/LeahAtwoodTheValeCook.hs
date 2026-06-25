module Arkham.Asset.Assets.LeahAtwoodTheValeCook (leahAtwoodTheValeCook) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher

newtype LeahAtwoodTheValeCook = LeahAtwoodTheValeCook AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leahAtwoodTheValeCook :: AssetCard LeahAtwoodTheValeCook
leahAtwoodTheValeCook =
  assetWith LeahAtwoodTheValeCook Cards.leahAtwoodTheValeCook $ (healthL ?~ 3) . (sanityL ?~ 1)

instance HasModifiersFor LeahAtwoodTheValeCook where
  getModifiersFor (LeahAtwoodTheValeCook a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities LeahAtwoodTheValeCook where
  getAbilities (LeahAtwoodTheValeCook a) =
    [ skillTestAbility $ restricted a 1 (OnSameLocation <> youCanTriggerCodex 2) parleyAction_
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage LeahAtwoodTheValeCook where
  runMessage msg a@(LeahAtwoodTheValeCook attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #combat (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 2
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      setCardAside attrs
      decreaseRelationshipLevel LeahAtwood 1
      pure a
    _ -> LeahAtwoodTheValeCook <$> liftRunMessage msg attrs
