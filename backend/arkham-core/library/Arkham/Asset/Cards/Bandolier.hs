module Arkham.Asset.Cards.Bandolier (
  Bandolier (..),
  bandolier,
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Trait

newtype Bandolier = Bandolier AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bandolier :: AssetCard Bandolier
bandolier = assetWith Bandolier Cards.bandolier (healthL ?~ 1)

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Weapon []

instance RunMessage Bandolier where
  runMessage msg (Bandolier attrs) = case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid HandSlot (slot attrs)
      Bandolier <$> runMessage msg attrs
    _ -> Bandolier <$> runMessage msg attrs
