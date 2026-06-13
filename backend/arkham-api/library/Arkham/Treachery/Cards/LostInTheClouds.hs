module Arkham.Treachery.Cards.LostInTheClouds (lostInTheClouds) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LostInTheClouds = LostInTheClouds TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheClouds :: TreacheryCard LostInTheClouds
lostInTheClouds = treachery LostInTheClouds Cards.lostInTheClouds

-- TODO: abilities
instance RunMessage LostInTheClouds where
  runMessage msg (LostInTheClouds attrs) = runQueueT $ LostInTheClouds <$> liftRunMessage msg attrs
