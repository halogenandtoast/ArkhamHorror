module Arkham.Asset.Assets.GoodMoney (goodMoney) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (taskEndsAbility)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (incrementRecordCountForInvestigator)

newtype GoodMoney = GoodMoney AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goodMoney :: AssetCard GoodMoney
goodMoney = asset GoodMoney Cards.goodMoney

instance HasAbilities GoodMoney where
  getAbilities (GoodMoney a) =
    [ -- "Place 1 of those resources on Good Money" moves a resource that is
      -- already in the pool, so this has to wait for #after; the #when window
      -- fires before the gain is applied.
      controlled a 1 (youExist InvestigatorWithAnyResources <> notYetBanked)
        $ freeReaction (GainsResources #after You AnySource (atLeast 1))
    , taskEndsAbility a (if banked then NoRestriction else Never)
    ]
   where
    banked = a.use #resource >= 5
    notYetBanked = whenOption "stopAtFiveResources" (if banked then Never else NoRestriction)

instance RunMessage GoodMoney where
  runMessage msg a@(GoodMoney attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) (ResourceSource iid) attrs #resource 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      incrementRecordCountForInvestigator iid Key.GoodMoney 1
      pure a
    _ -> GoodMoney <$> liftRunMessage msg attrs
