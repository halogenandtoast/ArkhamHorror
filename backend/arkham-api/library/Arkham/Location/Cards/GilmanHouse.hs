module Arkham.Location.Cards.GilmanHouse
  ( gilmanHouse
  , GilmanHouse(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GilmanHouse = GilmanHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gilmanHouse :: LocationCard GilmanHouse
gilmanHouse = location GilmanHouse Cards.gilmanHouse 2 (PerPlayer 1)

instance HasAbilities GilmanHouse where
  getAbilities (GilmanHouse attrs) =
    extendRevealed attrs []

instance RunMessage GilmanHouse where
  runMessage msg (GilmanHouse attrs) = runQueueT $ case msg of
    _ -> GilmanHouse <$> liftRunMessage msg attrs
