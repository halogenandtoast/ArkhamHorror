module Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (
  module Arkham.Campaigns.TheScarletKeys.Concealed.Helpers,
  module Arkham.Campaigns.TheScarletKeys.Concealed.Query,
) where

import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Concealed.Query
import Arkham.Campaigns.TheScarletKeys.I18n
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Helpers.Enemy
import Arkham.Helpers.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Query (getLead)
import Arkham.Id
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Monoid (First (..))

getConcealedIds :: HasGame m => ForExpose -> InvestigatorId -> m [ConcealedCardId]
getConcealedIds fe iid = map toId <$> getConcealed fe iid

getConcealed :: HasGame m => ForExpose -> InvestigatorId -> m [ConcealedCard]
getConcealed fe iid = getLocationOf iid >>= maybe (pure []) (getConcealedAt fe)

exposeConcealed
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> ConcealedCardId -> m ()
exposeConcealed iid source cid = doFlip iid source cid

revealConcealed
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> ConcealedCardId -> m ()
revealConcealed iid source cid = push $ Msg.LookAtRevealed iid (toSource source) (toTarget cid)

turnOverConcealed
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> ConcealedCardId -> m ()
turnOverConcealed iid source = push . Msg.DoStep 2 . Msg.LookAtRevealed iid (toSource source) . toTarget

turnOverAllConcealed :: (ReverseQueue m, Sourceable source) => source -> m ()
turnOverAllConcealed source = do
  lead <- getLead
  selectEach ConcealedCardAny \c -> turnOverConcealed lead source c.id

chooseExposeConcealed :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
chooseExposeConcealed iid source = chooseExposeConcealedAt iid source (LocationWithInvestigator $ InvestigatorWithId iid)

chooseExposeConcealedAt
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> LocationMatcher -> m ()
chooseExposeConcealedAt iid source lmatcher = do
  concealed <- getConcealedChoicesAt (ForExpose $ toSource source) lmatcher
  chooseOneM iid do
    targets concealed (exposeConcealed iid source . (.id))
    campaignI18n $ labeled' "doNotExposeConcealed" nothing

chooseRevealConcealedAt
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> LocationMatcher -> m ()
chooseRevealConcealedAt iid source lmatcher = do
  concealed <- getConcealedChoicesAt NotForExpose lmatcher
  chooseOneM iid do
    targets concealed (revealConcealed iid source . (.id))
    campaignI18n $ labeled' "doNotRevealConcealed" nothing

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

shuffleConcealedAt :: (ReverseQueue m, ToId location LocationId) => location -> m ()
shuffleConcealedAt location = do
  concealed <- getConcealedAtAll NotForExpose location
  let (known, unknown) = partition (attr concealedCardKnown) concealed
  case known <> unknown of
    (x : _) -> push $ Msg.PlaceConcealedCard "00000" x.id (AtLocation $ asId location)
    [] -> pure ()
