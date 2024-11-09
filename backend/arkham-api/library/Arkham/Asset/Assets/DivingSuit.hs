module Arkham.Asset.Assets.DivingSuit ( divingSuit , DivingSuit(..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DivingSuit = DivingSuit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

divingSuit :: AssetCard DivingSuit
divingSuit = asset DivingSuit Cards.divingSuit

instance RunMessage DivingSuit where
  runMessage msg (DivingSuit attrs) = runQueueT $ case msg of
    _ -> DivingSuit <$> liftRunMessage msg attrs
