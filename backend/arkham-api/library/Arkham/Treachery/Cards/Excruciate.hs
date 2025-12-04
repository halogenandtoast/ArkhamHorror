module Arkham.Treachery.Cards.Excruciate (excruciate) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Excruciate = Excruciate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

excruciate :: TreacheryCard Excruciate
excruciate = treachery Excruciate Cards.excruciate

instance RunMessage Excruciate where
  runMessage msg t@(Excruciate attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Excruciate <$> liftRunMessage msg attrs
