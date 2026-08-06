module Arkham.Asset.Assets.ProveYourWorth (proveYourWorth) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (taskEnds)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (incrementRecordCountForInvestigator)

newtype ProveYourWorth = ProveYourWorth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

proveYourWorth :: AssetCard ProveYourWorth
proveYourWorth = asset ProveYourWorth Cards.proveYourWorth

instance HasAbilities ProveYourWorth where
  getAbilities (ProveYourWorth a) =
    [ controlled a 1 (if a.use #chance > 0 then NoRestriction else Never)
        $ forced
        $ SkillTestResult #after You AnySkillTest (FailureResult $ atLeast 2)
    , controlled a 2 (if a.use #chance >= 1 then NoRestriction else Never)
        $ forced taskEnds
    ]

instance RunMessage ProveYourWorth where
  runMessage msg a@(ProveYourWorth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      spendUses (attrs.ability 1) attrs #chance 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      incrementRecordCountForInvestigator iid Key.ProveYourWorth 1
      pure a
    _ -> ProveYourWorth <$> liftRunMessage msg attrs
