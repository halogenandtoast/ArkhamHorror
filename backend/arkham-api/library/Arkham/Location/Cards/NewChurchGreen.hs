module Arkham.Location.Cards.NewChurchGreen
  ( newChurchGreen
  , NewChurchGreen(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype NewChurchGreen = NewChurchGreen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newChurchGreen :: LocationCard NewChurchGreen
newChurchGreen = location NewChurchGreen Cards.newChurchGreen 3 (PerPlayer 2)

instance HasAbilities NewChurchGreen where
  getAbilities (NewChurchGreen attrs) =
    extendRevealed attrs []

instance RunMessage NewChurchGreen where
  runMessage msg (NewChurchGreen attrs) = runQueueT $ case msg of
    _ -> NewChurchGreen <$> liftRunMessage msg attrs
