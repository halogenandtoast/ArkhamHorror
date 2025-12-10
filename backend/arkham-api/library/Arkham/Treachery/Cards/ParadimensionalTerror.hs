module Arkham.Treachery.Cards.ParadimensionalTerror (paradimensionalTerror) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ParadimensionalTerror = ParadimensionalTerror TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradimensionalTerror :: TreacheryCard ParadimensionalTerror
paradimensionalTerror = treachery ParadimensionalTerror Cards.paradimensionalTerror

instance RunMessage ParadimensionalTerror where
  runMessage msg t@(ParadimensionalTerror attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ParadimensionalTerror <$> liftRunMessage msg attrs
