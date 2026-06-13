module Arkham.Treachery.Cards.SeafloorFrieze (seafloorFrieze) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SeafloorFrieze = SeafloorFrieze TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seafloorFrieze :: TreacheryCard SeafloorFrieze
seafloorFrieze = treachery SeafloorFrieze Cards.seafloorFrieze

-- TODO: abilities
instance RunMessage SeafloorFrieze where
  runMessage msg (SeafloorFrieze attrs) = runQueueT $ SeafloorFrieze <$> liftRunMessage msg attrs
