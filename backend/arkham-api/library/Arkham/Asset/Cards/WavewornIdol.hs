module Arkham.Asset.Cards.WavewornIdol
  ( wavewornIdol
  , WavewornIdol(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype WavewornIdol = WavewornIdol AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wavewornIdol :: AssetCard WavewornIdol
wavewornIdol = asset WavewornIdol Cards.wavewornIdol

instance RunMessage WavewornIdol where
  runMessage msg (WavewornIdol attrs) = runQueueT $ case msg of
    _ -> WavewornIdol <$> liftRunMessage msg attrs
