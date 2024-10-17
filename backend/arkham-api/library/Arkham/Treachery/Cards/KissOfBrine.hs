module Arkham.Treachery.Cards.KissOfBrine
  ( kissOfBrine
  , KissOfBrine(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype KissOfBrine = KissOfBrine TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kissOfBrine :: TreacheryCard KissOfBrine
kissOfBrine = treachery KissOfBrine Cards.kissOfBrine

instance RunMessage KissOfBrine where
  runMessage msg t@(KissOfBrine attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> KissOfBrine <$> liftRunMessage msg attrs
