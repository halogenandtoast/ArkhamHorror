module Arkham.Treachery.Cards.Inundated
  ( inundated
  , Inundated(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Inundated = Inundated TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inundated :: TreacheryCard Inundated
inundated = treachery Inundated Cards.inundated

instance RunMessage Inundated where
  runMessage msg t@(Inundated attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Inundated <$> liftRunMessage msg attrs
