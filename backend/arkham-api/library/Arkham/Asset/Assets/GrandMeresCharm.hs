module Arkham.Asset.Assets.GrandMeresCharm (grandMeresCharm) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrandMeresCharm = GrandMeresCharm AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandMeresCharm :: AssetCard GrandMeresCharm
grandMeresCharm = assetWith GrandMeresCharm Cards.grandMeresCharm (healthL ?~ 3)

instance HasAbilities GrandMeresCharm where
  getAbilities (GrandMeresCharm x) =
    [ controlled x 1 (exists $ AssetAt YourLocation <> AssetWithUseType Charge <> AssetNotAtUsesX)
        $ freeTrigger (DirectDamageCost (toSource x) You 1 <> exhaust x)
    ]

instance RunMessage GrandMeresCharm where
  runMessage msg a@(GrandMeresCharm attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ at_ (locationWithInvestigator iid) <> AssetWithUseType Charge <> AssetNotAtUsesX
      chooseTargetM iid assets \aid ->
        addUses (attrs.ability 1) aid Charge 1
      pure a
    _ -> GrandMeresCharm <$> liftRunMessage msg attrs
