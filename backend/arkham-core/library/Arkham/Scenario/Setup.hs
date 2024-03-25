{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Scenario.Setup where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.RunMessage
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.EncounterSet
import Arkham.Id
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Phase
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
  :: CanRun m
  => (ScenarioAttrs -> b)
  -> ScenarioAttrs
  -> ScenarioBuilderT m ()
  -> m b
runScenarioSetup f attrs body = do
  f
    <$> execStateT
      ( body.unScenarioBuilderT
          >> shuffleEncounterDeck
          >> lift (pushAll [BeginGame, BeginRound, Begin InvestigationPhase])
      )
      attrs

shuffleEncounterDeck :: (MonadRandom m, MonadState ScenarioAttrs m) => m ()
shuffleEncounterDeck = do
  encounterDeck <- use encounterDeckL
  shuffledEncounterDeck <- withDeckM shuffleM encounterDeck
  encounterDeckL .= shuffledEncounterDeck

gather :: CardGen m => Set.EncounterSet -> ScenarioBuilderT m ()
gather encounterSet = do
  cards <- excludeBSides . excludeDoubleSided <$> gatherEncounterSet encounterSet
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
  setAsideCards defs
  encounterDeckL %= flip removeEachFromDeck defs

place :: ReverseQueue m => CardDef -> ScenarioBuilderT m LocationId
place def = do
  encounterDeckL %= flip removeEachFromDeck [def]
  placeLocationCard def

place_ :: ReverseQueue m => CardDef -> ScenarioBuilderT m ()
place_ = void . place

placeAll :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
placeAll defs = do
  encounterDeckL %= flip removeEachFromDeck defs
  placeLocationCards defs

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

addExtraDeck :: (ReverseQueue m, IsCard card) => ScenarioDeckKey -> [card] -> ScenarioBuilderT m ()
addExtraDeck k cards = do
  cards' <- shuffleM $ map toCard cards
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
