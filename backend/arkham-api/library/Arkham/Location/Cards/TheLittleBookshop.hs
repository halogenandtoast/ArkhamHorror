module Arkham.Location.Cards.TheLittleBookshop
  ( theLittleBookshop
  , TheLittleBookshop(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheLittleBookshop = TheLittleBookshop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLittleBookshop :: LocationCard TheLittleBookshop
theLittleBookshop = location TheLittleBookshop Cards.theLittleBookshop 2 (PerPlayer 2)

instance HasAbilities TheLittleBookshop where
  getAbilities (TheLittleBookshop attrs) =
    extendRevealed attrs []

instance RunMessage TheLittleBookshop where
  runMessage msg (TheLittleBookshop attrs) = runQueueT $ case msg of
    _ -> TheLittleBookshop <$> liftRunMessage msg attrs
