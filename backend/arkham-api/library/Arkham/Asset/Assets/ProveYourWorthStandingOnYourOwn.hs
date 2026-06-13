module Arkham.Asset.Assets.ProveYourWorthStandingOnYourOwn (proveYourWorthCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ProveYourWorthStandingOnYourOwn = ProveYourWorthStandingOnYourOwn AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
proveYourWorthCompleted :: AssetCard ProveYourWorthStandingOnYourOwn
proveYourWorthCompleted = asset ProveYourWorthStandingOnYourOwn Cards.proveYourWorthCompleted

instance RunMessage ProveYourWorthStandingOnYourOwn where
  runMessage msg (ProveYourWorthStandingOnYourOwn attrs) =
    runQueueT $ ProveYourWorthStandingOnYourOwn <$> liftRunMessage msg attrs
