module Arkham.Asset.Assets.ObsidianRelicPuzzlingEffigy (obsidianRelic) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ObsidianRelicPuzzlingEffigy = ObsidianRelicPuzzlingEffigy AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
obsidianRelic :: AssetCard ObsidianRelicPuzzlingEffigy
obsidianRelic = asset ObsidianRelicPuzzlingEffigy Cards.obsidianRelic

instance RunMessage ObsidianRelicPuzzlingEffigy where
  runMessage msg (ObsidianRelicPuzzlingEffigy attrs) =
    runQueueT $ ObsidianRelicPuzzlingEffigy <$> liftRunMessage msg attrs
