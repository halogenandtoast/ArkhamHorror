module Arkham.Location.Cards.WhiteBluff (whiteBluff) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WhiteBluff = WhiteBluff LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whiteBluff :: LocationCard WhiteBluff
whiteBluff = location WhiteBluff Cards.whiteBluff 3 (PerPlayer 1)

instance HasAbilities WhiteBluff where
  getAbilities (WhiteBluff attrs) =
    extendRevealed attrs []

instance RunMessage WhiteBluff where
  runMessage msg (WhiteBluff attrs) = runQueueT $ case msg of
    _ -> WhiteBluff <$> liftRunMessage msg attrs
