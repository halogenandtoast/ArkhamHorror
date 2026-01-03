module Arkham.Asset.Assets.RiverHawthorneBigInNewYork (riverHawthorneBigInNewYork) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)
import Arkham.Message.Lifted.Choose

newtype RiverHawthorneBigInNewYork = RiverHawthorneBigInNewYork AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverHawthorneBigInNewYork :: AssetCard RiverHawthorneBigInNewYork
riverHawthorneBigInNewYork = asset RiverHawthorneBigInNewYork Cards.riverHawthorneBigInNewYork

instance HasAbilities RiverHawthorneBigInNewYork where
  getAbilities (RiverHawthorneBigInNewYork a) =
    [groupLimit PerGame $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage RiverHawthorneBigInNewYork where
  runMessage msg a@(RiverHawthorneBigInNewYork attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid 5
      pure a
    _ -> RiverHawthorneBigInNewYork <$> liftRunMessage msg attrs
