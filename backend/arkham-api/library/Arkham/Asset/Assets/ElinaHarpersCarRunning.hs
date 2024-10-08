module Arkham.Asset.Assets.ElinaHarpersCarRunning
  ( elinaHarpersCarRunning
  , ElinaHarpersCarRunning(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ElinaHarpersCarRunning = ElinaHarpersCarRunning AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elinaHarpersCarRunning :: AssetCard ElinaHarpersCarRunning
elinaHarpersCarRunning = asset ElinaHarpersCarRunning Cards.elinaHarpersCarRunning

instance RunMessage ElinaHarpersCarRunning where
  runMessage msg (ElinaHarpersCarRunning attrs) = runQueueT $ case msg of
    _ -> ElinaHarpersCarRunning <$> liftRunMessage msg attrs
