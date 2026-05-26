module Arkham.Treachery.Cards.Incursion (incursion) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (Incursion)

newtype Incursion = Incursion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

incursion :: TreacheryCard Incursion
incursion = treachery Incursion Cards.incursion

instance RunMessage Incursion where
  runMessage msg t@(Incursion attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      pure t
    _ -> Incursion <$> liftRunMessage msg attrs
