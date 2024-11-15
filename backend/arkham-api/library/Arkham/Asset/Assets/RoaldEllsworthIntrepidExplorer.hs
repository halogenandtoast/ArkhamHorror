module Arkham.Asset.Assets.RoaldEllsworthIntrepidExplorer (
  roaldEllsworthIntrepidExplorer,
  RoaldEllsworthIntrepidExplorer (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype RoaldEllsworthIntrepidExplorer = RoaldEllsworthIntrepidExplorer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

roaldEllsworthIntrepidExplorer :: AssetCard RoaldEllsworthIntrepidExplorer
roaldEllsworthIntrepidExplorer = allyWith RoaldEllsworthIntrepidExplorer Cards.roaldEllsworthIntrepidExplorer (4, 2) noSlots

instance RunMessage RoaldEllsworthIntrepidExplorer where
  runMessage msg (RoaldEllsworthIntrepidExplorer attrs) = runQueueT $ case msg of
    _ -> RoaldEllsworthIntrepidExplorer <$> liftRunMessage msg attrs
