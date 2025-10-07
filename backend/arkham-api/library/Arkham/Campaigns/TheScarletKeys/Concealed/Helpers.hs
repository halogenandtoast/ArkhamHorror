module Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (
  module Arkham.Campaigns.TheScarletKeys.Concealed.Helpers,
  module Arkham.Campaigns.TheScarletKeys.Concealed.Query,
) where

import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Concealed.Query
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Source
import Arkham.Target

getConcealedIds :: HasGame m => InvestigatorId -> m [ConcealedCardId]
getConcealedIds iid = map toId <$> getConcealed iid

getConcealed :: HasGame m => InvestigatorId -> m [ConcealedCard]
getConcealed iid = getLocationOf iid >>= maybe (pure []) getConcealedAt

exposeConcealed
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> ConcealedCardId -> m ()
exposeConcealed iid source cid = doFlip iid source cid

revealConcealed
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> ConcealedCardId -> m ()
revealConcealed iid source cid = push $ Msg.LookAtRevealed iid (toSource source) (toTarget cid)

chooseExposeConcealed :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
chooseExposeConcealed iid source = chooseExposeConcealedAt iid source (LocationWithInvestigator $ InvestigatorWithId iid)

chooseExposeConcealedAt
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> LocationMatcher -> m ()
chooseExposeConcealedAt iid source lmatcher = do
  concealed <- getConcealedChoicesAt lmatcher
  chooseOneM iid do
    targets concealed $ doFlip iid source . toId
    labeled "Do not expose concealed" nothing
