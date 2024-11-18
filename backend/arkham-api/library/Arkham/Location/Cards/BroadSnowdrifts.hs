module Arkham.Location.Cards.BroadSnowdrifts (broadSnowdrifts, BroadSnowdrifts (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BroadSnowdrifts = BroadSnowdrifts LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

broadSnowdrifts :: LocationCard BroadSnowdrifts
broadSnowdrifts = symbolLabel $ location BroadSnowdrifts Cards.broadSnowdrifts 0 (Static 0)

instance HasAbilities BroadSnowdrifts where
  getAbilities (BroadSnowdrifts attrs) =
    extendRevealed attrs []

instance RunMessage BroadSnowdrifts where
  runMessage msg (BroadSnowdrifts attrs) = runQueueT $ case msg of
    _ -> BroadSnowdrifts <$> liftRunMessage msg attrs
