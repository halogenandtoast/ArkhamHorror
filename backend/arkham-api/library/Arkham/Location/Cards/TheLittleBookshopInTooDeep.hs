module Arkham.Location.Cards.TheLittleBookshopInTooDeep (
  theLittleBookshopInTooDeep,
  TheLittleBookshopInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype TheLittleBookshopInTooDeep = TheLittleBookshopInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLittleBookshopInTooDeep :: LocationCard TheLittleBookshopInTooDeep
theLittleBookshopInTooDeep =
  locationWith
    TheLittleBookshopInTooDeep
    Cards.theLittleBookshopInTooDeep
    3
    (Static 1)
    connectsToAdjacent

instance HasAbilities TheLittleBookshopInTooDeep where
  getAbilities (TheLittleBookshopInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage TheLittleBookshopInTooDeep where
  runMessage msg (TheLittleBookshopInTooDeep attrs) = runQueueT $ case msg of
    _ -> TheLittleBookshopInTooDeep <$> liftRunMessage msg attrs
