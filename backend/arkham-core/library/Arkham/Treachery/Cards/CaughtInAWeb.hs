module Arkham.Treachery.Cards.CaughtInAWeb
  ( caughtInAWeb
  , CaughtInAWeb(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaughtInAWeb = CaughtInAWeb TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caughtInAWeb :: TreacheryCard CaughtInAWeb
caughtInAWeb = treachery CaughtInAWeb Cards.caughtInAWeb

instance RunMessage CaughtInAWeb where
  runMessage msg t@(CaughtInAWeb attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> CaughtInAWeb <$> lift (runMessage msg attrs)
