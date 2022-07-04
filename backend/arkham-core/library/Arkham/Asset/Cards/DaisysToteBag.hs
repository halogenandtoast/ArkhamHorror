module Arkham.Asset.Cards.DaisysToteBag
  ( DaisysToteBag(..)
  , daisysToteBag
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Trait

newtype DaisysToteBag = DaisysToteBag AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBag :: AssetCard DaisysToteBag
daisysToteBag = asset DaisysToteBag Cards.daisysToteBag

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance RunMessage DaisysToteBag where
  runMessage msg (DaisysToteBag attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      pushAll $ replicate 2 (AddSlot iid HandSlot (slot attrs))
      DaisysToteBag <$> runMessage msg attrs
    _ -> DaisysToteBag <$> runMessage msg attrs
