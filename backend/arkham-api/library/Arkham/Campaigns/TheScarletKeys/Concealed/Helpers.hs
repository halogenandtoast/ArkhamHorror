module Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (
  module Arkham.Campaigns.TheScarletKeys.Concealed.Helpers,
  module Arkham.Campaigns.TheScarletKeys.Concealed.Query,
) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Concealed.Query
import Arkham.Campaigns.TheScarletKeys.I18n
import Arkham.Campaigns.TheScarletKeys.Modifiers
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Enemy
import Arkham.Helpers.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Query (getLead)
import Arkham.Id
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Window
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.Queue
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Data.Monoid (First (..))

getConcealedIds :: (HasGame m, Tracing m) => ForExpose -> InvestigatorId -> m [ConcealedCardId]
getConcealedIds fe iid = map toId <$> getConcealed fe iid

getConcealed :: (HasGame m, Tracing m) => ForExpose -> InvestigatorId -> m [ConcealedCard]
getConcealed fe iid = getLocationOf iid >>= maybe (pure []) (getConcealedAt fe)

exposeConcealed
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> ConcealedCardId -> m ()
exposeConcealed iid source cid = doFlip iid source cid

exposedInShadows
  :: (Sourceable a, HasCardCode a, ReverseQueue m) => InvestigatorId -> a -> QueueT Msg.Message m () -> m ()
exposedInShadows iid source = chooseOneM iid . abilityLabeled iid (mkAbility source (-1) $ forced AnyWindow)

moveFromShadows
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> ConcealedCardId -> m ()
moveFromShadows iid source cid = push $ Msg.DoStep 3 $ Msg.Flip iid (toSource source) (toTarget cid)

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
  when (notNull concealed) do
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
  :: (MonadRandom m, HasGame m, Tracing m) => EnemyId -> m (Maybe (ConcealedCardKind, [ConcealedCard]))
gatherConcealedCards a = do
  mconcealed <-
    getModifiedKeywords a <&> foldMap \case
      Keyword.Concealed card n -> First $ Just (card, n)
      _ -> First Nothing
  for (getFirst mconcealed) \(card, gv) -> do
    n <- getGameValue gv
    let
      cards =
        card
          : if card == VoidChimeraTrueForm
            then
              [ DecoyVoidChimeraFellbeak
              , DecoyVoidChimeraFellhound
              , DecoyVoidChimeraGorefeaster
              , DecoyVoidChimeraEarsplitter
              ]
            else replicate n Decoy
    (card,) <$> (shuffle cards >>= traverse mkConcealedCard)

makeDecoyAt :: ReverseQueue m => InvestigatorId -> LocationId -> m ()
makeDecoyAt iid loc = mkConcealedCard Decoy >>= \decoy -> makeDecoyAt' decoy iid loc

makeDecoyAt' :: ReverseQueue m => ConcealedCard -> InvestigatorId -> LocationId -> m ()
makeDecoyAt' decoy iid loc = do
  push $ Msg.CreateConcealedCard decoy
  push $ Msg.PlaceConcealedCards iid [decoy.id] [loc]

placeConcealed :: ReverseQueue m => InvestigatorId -> ConcealedCardKind -> [ConcealedCard] -> m ()
placeConcealed iid kind cards = do
  locations <-
    select $ LocationWithoutModifier $ CampaignModifier ("noConcealed[" <> tshow kind <> "]")
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

distributeEvenlyBetween :: (ReverseQueue m, ToId a ConcealedCardId) => [a] -> [LocationId] -> m ()
distributeEvenlyBetween concealed locations = do
  lead <- getLead
  do1 $ forTargets locations (Msg.PlaceConcealedCards lead (map asId concealed) locations)

pattern InvestigatorCanExpose :: InvestigatorMatcher
pattern InvestigatorCanExpose <- InvestigatorWithoutModifier CannotExpose
  where
    InvestigatorCanExpose = InvestigatorWithoutModifier CannotExpose

concealedToCardDef :: ConcealedCard -> Maybe CardDef
concealedToCardDef c = concealedKindToCardDef c.kind

concealedKindToCardDef :: ConcealedCardKind -> Maybe CardDef
concealedKindToCardDef = \case
  Decoy -> Nothing
  AcolyteAny -> Just Cards.acolyte
  ApportionedKa -> Just Cards.apportionedKa
  CoterieAgentA -> Just Cards.coterieAgentA
  CoterieAgentB -> Just Cards.coterieAgentB
  CoterieAgentC -> Just Cards.coterieAgentC
  EmissaryFromYuggoth -> Just Cards.emissaryFromYuggoth
  LaChicaRoja -> Just Cards.laChicaRojaTheGirlInTheCarmineCoat
  SinisterAspirantA -> Just Cards.sinisterAspirantA
  SinisterAspirantB -> Just Cards.sinisterAspirantB
  SinisterAspirantC -> Just Cards.sinisterAspirantC
  TheRedGlovedMan -> Just Cards.theRedGlovedManShroudedInMystery
  WizardOfTheOrder -> Just Cards.wizardOfTheOrder
  DesiderioDelgadoAlvarez -> Just Cards.desiderioDelgadoAlvarez106
  CoterieEnforcerA -> Just Cards.coterieEnforcerA
  CoterieEnforcerB -> Just Cards.coterieEnforcerB
  CoterieAssassinA -> Just Cards.coterieAssassinA
  CoterieAssassinB -> Just Cards.coterieAssassinB
  VoidChimeraTrueForm -> Just Cards.voidChimeraTrueForm
  TzuSanNiang -> Just Cards.tzuSanNiangTheLadyWithTheRedParasol
  _ -> Nothing

allConcealedCardDefs :: [CardDef]
allConcealedCardDefs = Cards.desiderioDelgadoAlvarez107 : mapMaybe concealedKindToCardDef [minBound .. maxBound]
