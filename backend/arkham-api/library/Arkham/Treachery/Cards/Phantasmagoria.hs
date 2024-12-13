module Arkham.Treachery.Cards.Phantasmagoria (phantasmagoria) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Phantasmagoria = Phantasmagoria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

phantasmagoria :: TreacheryCard Phantasmagoria
phantasmagoria = treachery Phantasmagoria Cards.phantasmagoria

instance RunMessage Phantasmagoria where
  runMessage msg t@(Phantasmagoria attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Phantasmagoria <$> liftRunMessage msg attrs
