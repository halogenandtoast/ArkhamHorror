module Arkham.Location.Cards.ReturnToCoyoacan (returnToCoyoacan) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToCoyoacan = ReturnToCoyoacan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCoyoacan :: LocationCard ReturnToCoyoacan
returnToCoyoacan = symbolLabel $ location ReturnToCoyoacan Cards.returnToCoyoacan 1 (Static 0)

instance HasAbilities ReturnToCoyoacan where
  getAbilities (ReturnToCoyoacan attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToCoyoacan where
  runMessage msg (ReturnToCoyoacan attrs) = runQueueT $ case msg of
    _ -> ReturnToCoyoacan <$> liftRunMessage msg attrs
