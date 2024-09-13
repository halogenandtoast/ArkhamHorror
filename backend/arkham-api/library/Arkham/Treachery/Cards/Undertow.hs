module Arkham.Treachery.Cards.Undertow
  ( undertow
  , Undertow(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Undertow = Undertow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undertow :: TreacheryCard Undertow
undertow = treachery Undertow Cards.undertow

instance RunMessage Undertow where
  runMessage msg t@(Undertow attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Undertow <$> liftRunMessage msg attrs
