module Arkham.Treachery.Cards.Anamnesis (anamnesis) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Anamnesis = Anamnesis TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anamnesis :: TreacheryCard Anamnesis
anamnesis = treachery Anamnesis Cards.anamnesis

instance RunMessage Anamnesis where
  runMessage msg t@(Anamnesis attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Anamnesis <$> liftRunMessage msg attrs
