module Arkham.Location.Cards.TeatroColon (teatroColon) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TeatroColon = TeatroColon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

teatroColon :: LocationCard TeatroColon
teatroColon = setLabel "f" $ location TeatroColon Cards.teatroColon 3 (PerPlayer 1)

instance HasAbilities TeatroColon where
  getAbilities (TeatroColon attrs) =
    extendRevealed attrs []

instance RunMessage TeatroColon where
  runMessage msg (TeatroColon attrs) = runQueueT $ case msg of
    _ -> TeatroColon <$> liftRunMessage msg attrs
