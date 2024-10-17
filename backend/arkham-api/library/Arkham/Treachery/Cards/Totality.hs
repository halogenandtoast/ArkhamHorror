module Arkham.Treachery.Cards.Totality
  ( totality
  , Totality(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Totality = Totality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

totality :: TreacheryCard Totality
totality = treachery Totality Cards.totality

instance RunMessage Totality where
  runMessage msg t@(Totality attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Totality <$> liftRunMessage msg attrs
