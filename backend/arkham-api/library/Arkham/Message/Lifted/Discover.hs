module Arkham.Message.Lifted.Discover where

import Arkham.Classes.HasQueue
import Arkham.Discover
import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Control.Monad.Trans

insteadOfDiscoveringClues
  :: (MonadTrans t, HasQueue Message m) => InvestigatorId -> (Discover -> t m ()) -> t m ()
insteadOfDiscoveringClues you f = runMaybeT_ do
  DoStep 1 (Arkham.Message.DiscoverClues _ n) <- MaybeT $ lift $ popMessageMatching \case
    DoStep 1 (Arkham.Message.DiscoverClues iid' _) -> you == iid'
    _ -> False
  lift $ f n
