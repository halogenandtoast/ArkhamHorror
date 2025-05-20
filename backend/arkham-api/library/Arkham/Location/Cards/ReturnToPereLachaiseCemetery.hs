module Arkham.Location.Cards.ReturnToPereLachaiseCemetery (returnToPereLachaiseCemetery) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToPereLachaiseCemetery = ReturnToPereLachaiseCemetery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToPereLachaiseCemetery :: LocationCard ReturnToPereLachaiseCemetery
returnToPereLachaiseCemetery = location ReturnToPereLachaiseCemetery Cards.returnToPereLachaiseCemetery 2 (PerPlayer 2)

instance HasAbilities ReturnToPereLachaiseCemetery where
  getAbilities (ReturnToPereLachaiseCemetery attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToPereLachaiseCemetery where
  runMessage msg (ReturnToPereLachaiseCemetery attrs) = runQueueT $ case msg of
    _ -> ReturnToPereLachaiseCemetery <$> liftRunMessage msg attrs
