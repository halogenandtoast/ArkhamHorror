module Arkham.Treachery.Cards.ParadimensionalUnderstanding (paradimensionalUnderstanding) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ParadimensionalUnderstanding = ParadimensionalUnderstanding TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradimensionalUnderstanding :: TreacheryCard ParadimensionalUnderstanding
paradimensionalUnderstanding = treachery ParadimensionalUnderstanding Cards.paradimensionalUnderstanding

instance RunMessage ParadimensionalUnderstanding where
  runMessage msg t@(ParadimensionalUnderstanding attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ParadimensionalUnderstanding <$> liftRunMessage msg attrs
