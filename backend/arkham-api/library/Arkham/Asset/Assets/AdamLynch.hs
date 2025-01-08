module Arkham.Asset.Assets.AdamLynch (adamLynch, AdamLynch (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Prelude

newtype AdamLynch = AdamLynch AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adamLynch :: AssetCard AdamLynch
adamLynch =
  allyWith AdamLynch Cards.adamLynch (1, 1)
    $ (isStoryL .~ True)
    . (slotsL .~ mempty)

instance HasAbilities AdamLynch where
  getAbilities (AdamLynch x) =
    [forcedAbility x 1 $ AssetLeavesPlay #when $ AssetWithId $ toId x]

instance HasModifiersFor AdamLynch where
  getModifiersFor (AdamLynch a) = case a.controller of
    Just iid ->
      selectOne (AbilityOnLocation $ LocationWithTitle "Security Office") >>= \case
        Just ab -> modified_ a (AbilityTarget iid ab) [ActionCostSetToModifier 1]
        _ -> pure mempty
    Nothing -> pure mempty

instance RunMessage AdamLynch where
  runMessage msg a@(AdamLynch attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pushAll [AddChaosToken Tablet, RemoveFromGame $ toTarget attrs]
      pure a
    _ -> AdamLynch <$> runMessage msg attrs
