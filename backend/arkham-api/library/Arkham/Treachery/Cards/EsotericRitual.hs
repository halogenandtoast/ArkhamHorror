module Arkham.Treachery.Cards.EsotericRitual (
  esotericRitual,
  EsotericRitual (..),
)
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EsotericRitual = EsotericRitual TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericRitual :: TreacheryCard EsotericRitual
esotericRitual = treachery EsotericRitual Cards.esotericRitual

instance RunMessage EsotericRitual where
  runMessage msg t@(EsotericRitual attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> EsotericRitual <$> liftRunMessage msg attrs
