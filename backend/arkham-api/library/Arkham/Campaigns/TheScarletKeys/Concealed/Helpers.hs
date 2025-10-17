module Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (
  module Arkham.Campaigns.TheScarletKeys.Concealed.Helpers,
  module Arkham.Campaigns.TheScarletKeys.Concealed.Query,
) where

import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Concealed.Query
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Helpers.Enemy
import Arkham.Helpers.GameValue
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Monoid (First (..))

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

gatherConcealedCards
  :: (MonadRandom m, HasGame m) => EnemyId -> m (Maybe (ConcealedCardKind, [ConcealedCard]))
gatherConcealedCards a = do
  mconcealed <-
    getModifiedKeywords a <&> foldMap \case
      Keyword.Concealed card n -> First $ Just (card, n)
      _ -> First Nothing
  for (getFirst mconcealed) \(card, gv) -> do
    n <- getGameValue gv
    (card,) <$> (shuffle (card : replicate n Decoy) >>= traverse mkConcealedCard)

placeConcealed :: ReverseQueue m => InvestigatorId -> ConcealedCardKind -> [ConcealedCard] -> m ()
placeConcealed iid kind cards = do
  locations <-
    select $ LocationWithoutModifier $ ScenarioModifier ("noConcealed[" <> tshow kind <> "]")
  for_ cards $ push . Msg.CreateConcealedCard
  push $ Msg.PlaceConcealedCards iid (map toId cards) locations

resolveConcealed :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
resolveConcealed iid eid =
  gatherConcealedCards eid >>= \case
    Nothing -> pure ()
    Just (kind, cards) -> placeConcealed iid kind cards
