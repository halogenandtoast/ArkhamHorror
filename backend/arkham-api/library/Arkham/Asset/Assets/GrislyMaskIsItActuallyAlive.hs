module Arkham.Asset.Assets.GrislyMaskIsItActuallyAlive (grislyMask) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype GrislyMaskIsItActuallyAlive = GrislyMaskIsItActuallyAlive AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
grislyMask :: AssetCard GrislyMaskIsItActuallyAlive
grislyMask = asset GrislyMaskIsItActuallyAlive Cards.grislyMask

instance RunMessage GrislyMaskIsItActuallyAlive where
  runMessage msg (GrislyMaskIsItActuallyAlive attrs) =
    runQueueT $ GrislyMaskIsItActuallyAlive <$> liftRunMessage msg attrs
