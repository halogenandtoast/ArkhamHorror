module Arkham.Treachery.Cards.HeraldsOfTheDeep
  ( heraldsOfTheDeep
  , HeraldsOfTheDeep(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HeraldsOfTheDeep = HeraldsOfTheDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heraldsOfTheDeep :: TreacheryCard HeraldsOfTheDeep
heraldsOfTheDeep = treachery HeraldsOfTheDeep Cards.heraldsOfTheDeep

instance RunMessage HeraldsOfTheDeep where
  runMessage msg t@(HeraldsOfTheDeep attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> HeraldsOfTheDeep <$> liftRunMessage msg attrs
