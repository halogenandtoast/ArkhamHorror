module Arkham.Location.Cards.CafeLunaBastionOfRemembrance (cafeLunaBastionOfRemembrance) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CafeLunaBastionOfRemembrance = CafeLunaBastionOfRemembrance LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cafeLunaBastionOfRemembrance :: LocationCard CafeLunaBastionOfRemembrance
cafeLunaBastionOfRemembrance = symbolLabel $ location CafeLunaBastionOfRemembrance Cards.cafeLunaBastionOfRemembrance 5 (PerPlayer 1)

instance HasAbilities CafeLunaBastionOfRemembrance where
  getAbilities (CafeLunaBastionOfRemembrance attrs) =
    extendRevealed attrs []

instance RunMessage CafeLunaBastionOfRemembrance where
  runMessage msg (CafeLunaBastionOfRemembrance attrs) = runQueueT $ case msg of
    _ -> CafeLunaBastionOfRemembrance <$> liftRunMessage msg attrs
