module Arkham.Treachery.Cards.Stowaway
  ( stowaway
  , Stowaway(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Stowaway = Stowaway TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stowaway :: TreacheryCard Stowaway
stowaway = treachery Stowaway Cards.stowaway

instance RunMessage Stowaway where
  runMessage msg t@(Stowaway attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Stowaway <$> liftRunMessage msg attrs
