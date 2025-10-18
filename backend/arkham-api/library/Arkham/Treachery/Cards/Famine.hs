module Arkham.Treachery.Cards.Famine (famine) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Famine = Famine TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

famine :: TreacheryCard Famine
famine = treachery Famine Cards.famine

instance RunMessage Famine where
  runMessage msg t@(Famine attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Famine <$> liftRunMessage msg attrs
