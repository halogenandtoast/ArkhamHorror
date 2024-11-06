module Arkham.Treachery.Cards.FulfillTheOaths
  ( fulfillTheOaths
  , FulfillTheOaths(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FulfillTheOaths = FulfillTheOaths TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fulfillTheOaths :: TreacheryCard FulfillTheOaths
fulfillTheOaths = treachery FulfillTheOaths Cards.fulfillTheOaths

instance RunMessage FulfillTheOaths where
  runMessage msg t@(FulfillTheOaths attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> FulfillTheOaths <$> liftRunMessage msg attrs
