module Arkham.Treachery.Cards.ICantSee
  ( iCantSee
  , ICantSee(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ICantSee = ICantSee TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iCantSee :: TreacheryCard ICantSee
iCantSee = treachery ICantSee Cards.iCantSee

instance RunMessage ICantSee where
  runMessage msg t@(ICantSee attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ICantSee <$> liftRunMessage msg attrs
