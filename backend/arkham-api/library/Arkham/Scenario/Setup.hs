{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Scenario.Setup where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.EncounterSet
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Prelude hiding ((.=))
import Arkham.Scenario.Helpers (excludeBSides, excludeDoubleSided)
import Arkham.Scenario.Runner ()
import Arkham.Scenario.Types
import Control.Lens
import Control.Monad.Random (MonadRandom (..))
import Control.Monad.State
import Data.List.NonEmpty qualified as NE

class SampleOneOf a where
  type Sampled a
  sampleOneOf :: MonadRandom m => a -> m (Sampled a)
  sampledFrom :: a -> [Sampled a]

instance SampleOneOf (a, a) where
  type Sampled (a, a) = a
  sampleOneOf (a, b) = sample2 a b
  sampledFrom (a, b) = [a, b]

instance SampleOneOf (a, a, a) where
  type Sampled (a, a, a) = a
  sampleOneOf (a, b, c) = sample (a :| [b, c])
  sampledFrom (a, b, c) = [a, b, c]

instance SampleOneOf (NonEmpty a) where
  type Sampled (NonEmpty a) = a
  sampleOneOf = sample
  sampledFrom = NE.toList

newtype ScenarioBuilderT m a = ScenarioBuilderT {unScenarioBuilderT :: StateT ScenarioAttrs m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState ScenarioAttrs, MonadTrans)

instance MonadRandom m => MonadRandom (ScenarioBuilderT m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

instance CardGen m => CardGen (ScenarioBuilderT m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cid = lift . replaceCard cid
  clearCardCache = lift clearCardCache

instance HasQueue Message m => HasQueue Message (ScenarioBuilderT m) where
  messageQueue = lift messageQueue
  pushAll = lift . pushAll

instance HasGame m => HasGame (ScenarioBuilderT m) where
  getGame = lift getGame

instance ReverseQueue m => ReverseQueue (ScenarioBuilderT m)

runScenarioSetup
  :: MonadRandom m
  => (ScenarioAttrs -> b)
  -> ScenarioAttrs
  -> ScenarioBuilderT m ()
  -> m b
runScenarioSetup f attrs body =
  f <$> execStateT (body.unScenarioBuilderT >> shuffleEncounterDeck) attrs

shuffleEncounterDeck :: (MonadRandom m, MonadState ScenarioAttrs m) => m ()
shuffleEncounterDeck = do
  encounterDeck <- use encounterDeckL
  shuffledEncounterDeck <- withDeckM shuffleM encounterDeck
  encounterDeckL .= shuffledEncounterDeck

gather :: CardGen m => Set.EncounterSet -> ScenarioBuilderT m ()
gather encounterSet = do
  cards <- excludeBSides . excludeDoubleSided <$> gatherEncounterSet encounterSet
  encounterDeckL %= (Deck cards <>)

gatherJust :: CardGen m => Set.EncounterSet -> [CardDef] -> ScenarioBuilderT m ()
gatherJust encounterSet defs = do
  cards <-
    filter ((`cardMatch` mapOneOf cardIs defs) . toCard)
      . excludeBSides
      . excludeDoubleSided
      <$> gatherEncounterSet encounterSet
  encounterDeckL %= (Deck cards <>)

gatherAndSetAside :: ReverseQueue m => Set.EncounterSet -> ScenarioBuilderT m ()
gatherAndSetAside encounterSet = do
  cards <- map toCard <$> gatherEncounterSet encounterSet
  push $ SetAsideCards cards

gatherOneOf
  :: (SampleOneOf as, Sampled as ~ Set.EncounterSet, CardGen m) => as -> ScenarioBuilderT m ()
gatherOneOf = sampleOneOf >=> gather

setAside :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
setAside defs = do
  cards <- genCards defs
  setAsideCardsL %= (<> cards)
  encounterDeckL %= flip removeEachFromDeck defs

removeEvery :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
removeEvery defs = encounterDeckL %= flip removeEveryFromDeck defs

fromSetAside :: (HasCallStack, ReverseQueue m) => CardDef -> ScenarioBuilderT m Card
fromSetAside def = do
  cards <- use setAsideCardsL
  case find ((== def) . toCardDef) cards of
    Just card -> do
      setAsideCardsL %= filter (/= card)
      pure card
    Nothing -> error $ "Card " <> show def <> " not found in set aside cards"

amongGathered :: (HasCallStack, ReverseQueue m) => CardMatcher -> ScenarioBuilderT m [Card]
amongGathered matcher = filterCards matcher . map toCard . unDeck <$> use encounterDeckL

place :: ReverseQueue m => CardDef -> ScenarioBuilderT m LocationId
place def = do
  encounterDeckL %= flip removeEachFromDeck [def]
  placeLocationCard def

placeLabeled_ :: ReverseQueue m => Text -> CardDef -> ScenarioBuilderT m ()
placeLabeled_ lbl def = void $ placeLabeled lbl def

placeLabeled :: ReverseQueue m => Text -> CardDef -> ScenarioBuilderT m LocationId
placeLabeled lbl def = do
  encounterDeckL %= flip removeEachFromDeck [def]
  lid <- placeLocationCard def
  push $ SetLocationLabel lid lbl
  pure lid

place_ :: ReverseQueue m => CardDef -> ScenarioBuilderT m ()
place_ = void . place

placeAll :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
placeAll defs = do
  encounterDeckL %= flip removeEachFromDeck defs
  placeLocationCards defs

placeAllCapture :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m [LocationId]
placeAllCapture defs = traverse place defs

placeOneOf
  :: (SampleOneOf as, Sampled as ~ CardDef, ReverseQueue m) => as -> ScenarioBuilderT m LocationId
placeOneOf as = do
  def <- sampleOneOf as
  encounterDeckL %= flip removeEachFromDeck (sampledFrom as)
  placeLocationCard def

placeOneOf_
  :: (SampleOneOf as, Sampled as ~ CardDef, ReverseQueue m) => as -> ScenarioBuilderT m ()
placeOneOf_ = void . placeOneOf

placeGroup :: ReverseQueue m => Text -> [CardDef] -> ScenarioBuilderT m ()
placeGroup groupName defs = do
  encounterDeckL %= flip removeEachFromDeck defs
  placeRandomLocationGroupCards groupName defs

placeGroupChooseN :: ReverseQueue m => Int -> Text -> NonEmpty CardDef -> ScenarioBuilderT m ()
placeGroupChooseN n groupName = sampleN n >=> placeGroup groupName

startAt :: ReverseQueue m => LocationId -> ScenarioBuilderT m ()
startAt lid = do
  reveal lid
  attrs <- get
  moveAllTo attrs lid

addToEncounterDeck
  :: (ReverseQueue m, MonoFoldable defs, HasCardDef (Element defs)) => defs -> ScenarioBuilderT m ()
addToEncounterDeck (toList -> defs) = do
  cards <- traverse genEncounterCard defs
  encounterDeckL %= withDeck (<> cards)

enemyAt :: ReverseQueue m => CardDef -> LocationId -> ScenarioBuilderT m ()
enemyAt def lid = do
  encounterDeckL %= flip removeEachFromDeck [def]
  card <- genCard def
  createEnemyAt_ card lid

enemyAtMatching :: ReverseQueue m => CardDef -> LocationMatcher -> ScenarioBuilderT m ()
enemyAtMatching def matcher = do
  encounterDeckL %= flip removeEachFromDeck [def]
  card <- genCard def
  createEnemyAtLocationMatching_ card matcher

sampleEncounterDeck :: (HasCallStack, MonadRandom m) => Int -> ScenarioBuilderT m [EncounterCard]
sampleEncounterDeck n = do
  deck <- use encounterDeckL
  case nonEmpty (unDeck deck) of
    Nothing -> error "expected the encounter deck not to be empty"
    Just ne -> do
      cards <- sampleN n ne
      encounterDeckL %= withDeck (filter (`notElem` cards))
      pure cards

addExtraDeck :: (ReverseQueue m, IsCard card) => ScenarioDeckKey -> [card] -> ScenarioBuilderT m ()
addExtraDeck k cards = do
  cards' <- shuffleM $ map toCard cards
  encounterDeckL %= withDeck (filter ((`notElem` cards') . toCard))
  decksL %= (at k ?~ cards')

setActDeck :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
setActDeck defs = do
  cards <- genCards defs
  actStackL %= insertMap 1 cards
  push SetActDeck

setAgendaDeck :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
setAgendaDeck defs = do
  cards <- genCards defs
  agendaStackL %= insertMap 1 cards
  push SetAgendaDeck

setAgendaDeckN :: ReverseQueue m => Int -> [CardDef] -> ScenarioBuilderT m ()
setAgendaDeckN n defs = do
  cards <- genCards defs
  agendaStackL %= insertMap n cards
  push SetAgendaDeck

placeUnderScenarioReference :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
placeUnderScenarioReference defs = do
  cards <- genCards defs
  cardsUnderScenarioReferenceL %= (<> cards)
