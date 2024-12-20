module Arkham.Treachery.Cards.EvanescentMist (evanescentMist) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EvanescentMist = EvanescentMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evanescentMist :: TreacheryCard EvanescentMist
evanescentMist = treachery EvanescentMist Cards.evanescentMist

instance RunMessage EvanescentMist where
  runMessage msg t@(EvanescentMist attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> EvanescentMist <$> liftRunMessage msg attrs
