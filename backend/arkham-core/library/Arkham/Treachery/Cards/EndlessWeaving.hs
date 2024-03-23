module Arkham.Treachery.Cards.EndlessWeaving
  ( endlessWeaving
  , EndlessWeaving(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EndlessWeaving = EndlessWeaving TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessWeaving :: TreacheryCard EndlessWeaving
endlessWeaving = treachery EndlessWeaving Cards.endlessWeaving

instance RunMessage EndlessWeaving where
  runMessage msg t@(EndlessWeaving attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> EndlessWeaving <$> lift (runMessage msg attrs)
