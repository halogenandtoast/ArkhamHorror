module Arkham.Location.Cards.WindsweptPath (windsweptPath) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WindsweptPath = WindsweptPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

windsweptPath :: LocationCard WindsweptPath
windsweptPath = location WindsweptPath Cards.windsweptPath 3 (Static 0)

instance HasAbilities WindsweptPath where
  getAbilities (WindsweptPath attrs) =
    extendRevealed attrs []

instance RunMessage WindsweptPath where
  runMessage msg (WindsweptPath attrs) = runQueueT $ case msg of
    _ -> WindsweptPath <$> liftRunMessage msg attrs
