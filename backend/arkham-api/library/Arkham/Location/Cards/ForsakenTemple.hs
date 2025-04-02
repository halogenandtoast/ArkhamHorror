module Arkham.Location.Cards.ForsakenTemple (forsakenTemple) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForsakenTemple = ForsakenTemple LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTemple :: LocationCard ForsakenTemple
forsakenTemple = location ForsakenTemple Cards.forsakenTemple 3 (PerPlayer 1)

instance HasAbilities ForsakenTemple where
  getAbilities (ForsakenTemple attrs) =
    extendRevealed attrs []

instance RunMessage ForsakenTemple where
  runMessage msg (ForsakenTemple attrs) = runQueueT $ case msg of
    _ -> ForsakenTemple <$> liftRunMessage msg attrs
