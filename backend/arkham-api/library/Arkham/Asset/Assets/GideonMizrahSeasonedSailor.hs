module Arkham.Asset.Assets.GideonMizrahSeasonedSailor (gideonMizrahSeasonedSailor) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Matcher
import Arkham.Slot

newtype GideonMizrahSeasonedSailor = GideonMizrahSeasonedSailor AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gideonMizrahSeasonedSailor :: AssetCard GideonMizrahSeasonedSailor
gideonMizrahSeasonedSailor =
  assetWith GideonMizrahSeasonedSailor Cards.gideonMizrahSeasonedSailor
    $ (healthL ?~ 1)
    . (sanityL ?~ 3)

instance HasAbilities GideonMizrahSeasonedSailor where
  getAbilities (GideonMizrahSeasonedSailor a) =
    [ restricted a 1 (OnSameLocation <> youCanTriggerCodex 6) (parleyAction (ActionCost 1))
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage GideonMizrahSeasonedSailor where
  runMessage msg a@(GideonMizrahSeasonedSailor attrs) = runQueueT $ case msg of
    TakeControlOfAsset iid (is attrs -> True) -> do
      push $ AddSlot iid #accessory (Slot (toSource attrs) [])
      GideonMizrahSeasonedSailor <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 6
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      setCardAside attrs
      decreaseRelationshipLevel GideonMizrah 1
      pure a
    _ -> GideonMizrahSeasonedSailor <$> liftRunMessage msg attrs
