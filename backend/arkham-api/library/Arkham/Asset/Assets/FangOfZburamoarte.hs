module Arkham.Asset.Assets.FangOfZburamoarte (fangOfZburamoarte) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype FangOfZburamoarte = FangOfZburamoarte AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fangOfZburamoarte :: AssetCard FangOfZburamoarte
fangOfZburamoarte = asset FangOfZburamoarte Cards.fangOfZburamoarte

instance RunMessage FangOfZburamoarte where
  runMessage msg (FangOfZburamoarte attrs) = runQueueT $ case msg of
    _ -> FangOfZburamoarte <$> liftRunMessage msg attrs
