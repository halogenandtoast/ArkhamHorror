module Arkham.Asset.Assets.GideonMizrahSeasonedSailor (gideonMizrahSeasonedSailor) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (Resident (..), codex, decreaseRelationshipLevel)

newtype GideonMizrahSeasonedSailor = GideonMizrahSeasonedSailor AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gideonMizrahSeasonedSailor :: AssetCard GideonMizrahSeasonedSailor
gideonMizrahSeasonedSailor = assetWith GideonMizrahSeasonedSailor Cards.gideonMizrahSeasonedSailor $ (healthL ?~ 1) . (sanityL ?~ 3)

instance HasModifiersFor GideonMizrahSeasonedSailor where
  getModifiersFor (GideonMizrahSeasonedSailor a) = controllerGets a [AdditionalSlot #accessory]

instance HasAbilities GideonMizrahSeasonedSailor where
  getAbilities (GideonMizrahSeasonedSailor a) =
    [ mkAbility a 99 $ forced $ AssetDefeated #when ByAny (be a)
    ,groupLimit PerGame $ restricted a 1 OnSameLocation doubleActionAbility]

instance RunMessage GideonMizrahSeasonedSailor where
  runMessage msg a@(GideonMizrahSeasonedSailor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 6
      pure a
    UseCardAbility _ (isSource attrs -> True) 99 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      push $ SetAsideCards [toCard attrs]
      decreaseRelationshipLevel GideonMizrah 1
      pure a
    _ -> GideonMizrahSeasonedSailor <$> liftRunMessage msg attrs
