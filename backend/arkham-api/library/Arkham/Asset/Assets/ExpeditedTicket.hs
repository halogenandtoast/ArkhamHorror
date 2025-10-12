module Arkham.Asset.Assets.ExpeditedTicket (expeditedTicket) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ExpeditedTicket = ExpeditedTicket AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditedTicket :: AssetCard ExpeditedTicket
expeditedTicket = asset ExpeditedTicket Cards.expeditedTicket

instance RunMessage ExpeditedTicket where
  runMessage msg (ExpeditedTicket attrs) = runQueueT $ case msg of
    _ -> ExpeditedTicket <$> liftRunMessage msg attrs
