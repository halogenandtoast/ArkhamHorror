module Arkham.Asset.Assets.EliyahAshevakDogHandler (
  eliyahAshevakDogHandler,
  EliyahAshevakDogHandler (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype EliyahAshevakDogHandler = EliyahAshevakDogHandler AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eliyahAshevakDogHandler :: AssetCard EliyahAshevakDogHandler
eliyahAshevakDogHandler = allyWith EliyahAshevakDogHandler Cards.eliyahAshevakDogHandler (3, 3) noSlots

instance RunMessage EliyahAshevakDogHandler where
  runMessage msg (EliyahAshevakDogHandler attrs) = runQueueT $ case msg of
    _ -> EliyahAshevakDogHandler <$> liftRunMessage msg attrs
