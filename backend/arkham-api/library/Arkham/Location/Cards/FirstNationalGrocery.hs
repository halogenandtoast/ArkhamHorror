module Arkham.Location.Cards.FirstNationalGrocery
  ( firstNationalGrocery
  , FirstNationalGrocery(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FirstNationalGrocery = FirstNationalGrocery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstNationalGrocery :: LocationCard FirstNationalGrocery
firstNationalGrocery = location FirstNationalGrocery Cards.firstNationalGrocery 3 (PerPlayer 1)

instance HasAbilities FirstNationalGrocery where
  getAbilities (FirstNationalGrocery attrs) =
    extendRevealed attrs []

instance RunMessage FirstNationalGrocery where
  runMessage msg (FirstNationalGrocery attrs) = runQueueT $ case msg of
    _ -> FirstNationalGrocery <$> liftRunMessage msg attrs
