module Arkham.Asset.Assets.ArcaneEnlightenment (
  ArcaneEnlightenment (..),
  arcaneEnlightenment,
) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Prelude
import Arkham.Trait

newtype ArcaneEnlightenment = ArcaneEnlightenment AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneEnlightenment :: AssetCard ArcaneEnlightenment
arcaneEnlightenment = asset ArcaneEnlightenment Cards.arcaneEnlightenment

instance HasModifiersFor ArcaneEnlightenment where
  getModifiersFor (ArcaneEnlightenment a) = case a.controller of
    Just iid -> modified_ a iid [HandSize 1]
    Nothing -> pure mempty

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome []

instance RunMessage ArcaneEnlightenment where
  runMessage msg (ArcaneEnlightenment attrs) = case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push (AddSlot iid HandSlot (slot attrs))
      ArcaneEnlightenment <$> runMessage msg attrs
    _ -> ArcaneEnlightenment <$> runMessage msg attrs
