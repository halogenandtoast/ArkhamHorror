module Arkham.Treachery.Cards.CthulhuFhtagn (cthulhuFhtagn) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CthulhuFhtagn = CthulhuFhtagn TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuFhtagn :: TreacheryCard CthulhuFhtagn
cthulhuFhtagn = treachery CthulhuFhtagn Cards.cthulhuFhtagn

-- TODO: abilities
instance RunMessage CthulhuFhtagn where
  runMessage msg (CthulhuFhtagn attrs) = runQueueT $ CthulhuFhtagn <$> liftRunMessage msg attrs
