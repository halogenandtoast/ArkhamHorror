module Arkham.Asset.Assets.ElinaHarpersCarStopped
  ( elinaHarpersCarStopped
  , ElinaHarpersCarStopped(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ElinaHarpersCarStopped = ElinaHarpersCarStopped AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elinaHarpersCarStopped :: AssetCard ElinaHarpersCarStopped
elinaHarpersCarStopped = asset ElinaHarpersCarStopped Cards.elinaHarpersCarStopped

instance RunMessage ElinaHarpersCarStopped where
  runMessage msg (ElinaHarpersCarStopped attrs) = runQueueT $ case msg of
    _ -> ElinaHarpersCarStopped <$> liftRunMessage msg attrs
