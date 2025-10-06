module Arkham.Campaigns.TheScarletKeys.Concealed.Helpers where

import Arkham.Classes.HasGame
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Location.Types (Field (..))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source

getConcealed :: HasGame m => InvestigatorId -> m (Maybe ConcealedCardId)
getConcealed iid = runMaybeT $ getConcealedT iid

getConcealedT :: HasGame m => InvestigatorId -> MaybeT m ConcealedCardId
getConcealedT iid = MaybeT (getLocationOf iid) >>= MaybeT . getConcealedAt

getConcealedAt :: HasGame m => LocationId -> m (Maybe ConcealedCardId)
getConcealedAt = fieldMap LocationConcealedCards headMay

exposeConcealed
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> ConcealedCardId -> m ()
exposeConcealed iid source cid = doFlip iid source cid

chooseExposeConcealed :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
chooseExposeConcealed iid source = whenJustM (getConcealed iid) \cid -> do
  chooseOneM iid do
    labeled "Expose concealed" $ doFlip iid source cid
    labeled "Do not expose concealed" nothing
