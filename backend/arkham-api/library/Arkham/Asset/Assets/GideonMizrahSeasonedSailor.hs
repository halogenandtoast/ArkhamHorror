module Arkham.Asset.Assets.GideonMizrahSeasonedSailor (gideonMizrahSeasonedSailor) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)

newtype GideonMizrahSeasonedSailor = GideonMizrahSeasonedSailor AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gideonMizrahSeasonedSailor :: AssetCard GideonMizrahSeasonedSailor
gideonMizrahSeasonedSailor = asset GideonMizrahSeasonedSailor Cards.gideonMizrahSeasonedSailor

instance HasAbilities GideonMizrahSeasonedSailor where
  getAbilities (GideonMizrahSeasonedSailor a) =
    [groupLimit PerGame $ restricted a 1 OnSameLocation doubleActionAbility]

instance RunMessage GideonMizrahSeasonedSailor where
  runMessage msg a@(GideonMizrahSeasonedSailor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid 6
      pure a
    _ -> GideonMizrahSeasonedSailor <$> liftRunMessage msg attrs
