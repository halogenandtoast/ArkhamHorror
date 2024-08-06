module Arkham.Treachery.Cards.DreadCurse (dreadCurse, DreadCurse (..)) where

import Arkham.Helpers.ChaosBag
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DreadCurse = DreadCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreadCurse :: TreacheryCard DreadCurse
dreadCurse = treachery DreadCurse Cards.dreadCurse

instance RunMessage DreadCurse where
  runMessage msg t@(DreadCurse attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      n <- min 5 <$> getRemainingCurseTokens
      addCurseTokens n 
      pure t
    _ -> DreadCurse <$> liftRunMessage msg attrs
