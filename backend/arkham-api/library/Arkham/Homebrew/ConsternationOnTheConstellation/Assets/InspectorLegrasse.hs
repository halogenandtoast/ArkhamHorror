module Arkham.Homebrew.ConsternationOnTheConstellation.Assets.InspectorLegrasse (inspectorLegrasse) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Assets qualified as Cards

newtype InspectorLegrasse = InspectorLegrasse AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The scenario's reward, offered by "Additional Rewards" when the Constellation is
saved. You gain an extra accessory slot and an extra hand slot, both restricted
to Relic assets. After a Cultist at your location is defeated, exhaust him to
discover a clue or gain 2 resources.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
inspectorLegrasse :: AssetCard InspectorLegrasse
inspectorLegrasse = ally InspectorLegrasse Cards.inspectorLegrasse (2, 2)

instance RunMessage InspectorLegrasse where
  runMessage msg (InspectorLegrasse attrs) = runQueueT $ InspectorLegrasse <$> liftRunMessage msg attrs
