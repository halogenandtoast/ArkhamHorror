module Arkham.Treachery.Cards.StElmosFire (stElmosFire) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StElmosFire = StElmosFire TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stElmosFire :: TreacheryCard StElmosFire
stElmosFire = treachery StElmosFire Cards.stElmosFire

-- TODO: abilities
instance RunMessage StElmosFire where
  runMessage msg (StElmosFire attrs) = runQueueT $ StElmosFire <$> liftRunMessage msg attrs
