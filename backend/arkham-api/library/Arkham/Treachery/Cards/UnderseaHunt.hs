module Arkham.Treachery.Cards.UnderseaHunt (underseaHunt) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnderseaHunt = UnderseaHunt TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaHunt :: TreacheryCard UnderseaHunt
underseaHunt = treachery UnderseaHunt Cards.underseaHunt

-- TODO: abilities
instance RunMessage UnderseaHunt where
  runMessage msg (UnderseaHunt attrs) = runQueueT $ UnderseaHunt <$> liftRunMessage msg attrs
