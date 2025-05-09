module Arkham.Asset.Assets.AdamLynch (adamLynch) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher

newtype AdamLynch = AdamLynch AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adamLynch :: AssetCard AdamLynch
adamLynch = allyWith AdamLynch Cards.adamLynch (1, 1) noSlots

instance HasAbilities AdamLynch where
  getAbilities (AdamLynch x) = [forcedAbility x 1 $ AssetLeavesPlay #when (be x)]

instance HasModifiersFor AdamLynch where
  getModifiersFor (AdamLynch a) = for_ a.controller \iid -> do
    selectForMaybeM (AbilityOnLocation (LocationWithTitle "Security Office") <> AbilityWithIndex 1) \ab ->
      modified_ a (AbilityTarget iid ab.ref) [ActionCostSetToModifier 1]

instance RunMessage AdamLynch where
  runMessage msg a@(AdamLynch attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addChaosToken Tablet
      removeFromGame attrs
      pure a
    _ -> AdamLynch <$> liftRunMessage msg attrs
