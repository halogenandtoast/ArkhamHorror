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
import Arkham.Key
import Arkham.Layout
import Arkham.Location.Grid
import Arkham.Matcher hiding (assetAt)
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveAllTo)
import Arkham.Message.Lifted.Placement (IsPlacement (..))
import Arkham.Placement
import Arkham.Prelude hiding ((.=))
import Arkham.Scenario.Helpers (excludeBSides, excludeDoubleSided, getLead, hasBSide, isDoubleSided)
import Arkham.Scenario.Runner (createEnemyWithPlacement_, pushM)
import Arkham.Scenario.Types
import Arkham.ScenarioLogKey
import Arkham.Token (Token, addTokens)
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

data ScenarioBuilderState = ScenarioBuilderState
  { attrs :: ScenarioAttrs
  , otherCards :: [Card]
  }

attrsL :: Lens' ScenarioBuilderState ScenarioAttrs
attrsL = lens (.attrs) \m x -> m {attrs = x}

otherCardsL :: Lens' ScenarioBuilderState [Card]
otherCardsL = lens (.otherCards) \m x -> m {otherCards = x}

newtype ScenarioBuilderT m a = ScenarioBuilderT {unScenarioBuilderT :: StateT ScenarioBuilderState m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState ScenarioBuilderState, MonadTrans)

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
  f
    . (.attrs)
    <$> execStateT
      (clearCards >> body.unScenarioBuilderT >> shuffleEncounterDeck)
      (ScenarioBuilderState attrs [])

shuffleEncounterDeck :: (MonadRandom m, MonadState ScenarioBuilderState m) => m ()
shuffleEncounterDeck = do
  encounterDeck <- use (attrsL . encounterDeckL)
  shuffledEncounterDeck <- withDeckM shuffleM encounterDeck
  attrsL . encounterDeckL .= shuffledEncounterDeck

clearCards :: MonadState ScenarioBuilderState m => m ()
clearCards = do
  attrsL . encounterDeckL .= Deck []
  attrsL . discardL .= []

gather :: CardGen m => Set.EncounterSet -> ScenarioBuilderT m ()
gather encounterSet = do
  (other, cards) <-
    partition (or . sequence [isDoubleSided, hasBSide]) <$> gatherEncounterSet encounterSet
  attrsL . encounterDeckL %= (Deck cards <>)
  otherCardsL %= (map toCard other <>)

gatherJust :: CardGen m => Set.EncounterSet -> [CardDef] -> ScenarioBuilderT m ()
gatherJust encounterSet defs = do
  cards <-
    filter ((`cardMatch` mapOneOf cardIs defs) . toCard)
      . excludeBSides
      . excludeDoubleSided
      <$> gatherEncounterSet encounterSet
  attrsL . encounterDeckL %= (Deck cards <>)

gatherAndSetAside :: ReverseQueue m => Set.EncounterSet -> ScenarioBuilderT m ()
gatherAndSetAside encounterSet = do
  cards <- map toCard <$> gatherEncounterSet encounterSet
  push $ SetAsideCards cards

gatherOneOf
  :: (SampleOneOf as, Sampled as ~ Set.EncounterSet, CardGen m) => as -> ScenarioBuilderT m ()
gatherOneOf = sampleOneOf >=> gather

setAsideKeys :: ReverseQueue m => [ArkhamKey] -> ScenarioBuilderT m ()
setAsideKeys ks = attrsL . setAsideKeysL %= (<> setFromList ks)

setAsideKey :: ReverseQueue m => ArkhamKey -> ScenarioBuilderT m ()
setAsideKey k = attrsL . setAsideKeysL %= (<> singleton k)

placeStory :: ReverseQueue m => CardDef -> ScenarioBuilderT m ()
placeStory def = do
  card <- genCard def
  push $ PlaceStory card Global

setAside :: (ReverseQueue m, FindInEncounterDeck a) => [a] -> ScenarioBuilderT m ()
setAside as = do
  cards <- for as \a -> do
    deck <- use (attrsL . encounterDeckL)
    case findInDeck a deck of
      Just card -> do
        attrsL . encounterDeckL %= filter (/= card)
        pure $ toCard card
      Nothing -> do
        card <- notFoundInDeck a
        otherCardsL %= filter (/= toCard card)

        for_ (cdOtherSide $ toCardDef card) \otherSide -> do
          otherCardsBefore <- use otherCardsL
          otherCardsL %= filter ((`notElem` [otherSide, toCardCode card]) . toCardCode)

          otherCardsAfter <- use otherCardsL
          when (otherCardsBefore == otherCardsAfter) do
            error $ "Card not found in encounter deck or other cards: " <> (show $ cdOtherSide $ toCardDef card)
        pure card

  attrsL . setAsideCardsL %= (<> cards)
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck (map toCardDef cards)

-- setAside :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
-- setAside defs = do
--   setAsideCards defs
--   encounterDeckL %= flip removeEachFromDeck defs

-- does not handle other encounter decks
removeEvery :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
removeEvery defs = attrsL . encounterDeckL %= flip removeEveryFromDeck defs

-- does not handle other encounter decks
removeOneOf :: ReverseQueue m => CardDef -> ScenarioBuilderT m ()
removeOneOf def = removeOneOfEach [def]

-- does not handle other encounter decks
removeOneOfEach :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
removeOneOfEach defs = attrsL . encounterDeckL %= flip removeEachFromDeck defs

fromSetAside :: (HasCallStack, ReverseQueue m) => CardDef -> ScenarioBuilderT m Card
fromSetAside def = do
  cards <- use (attrsL . setAsideCardsL)
  case find ((== def) . toCardDef) cards of
    Just card -> do
      attrsL . setAsideCardsL %= filter (/= card)
      pure card
    Nothing -> error $ "Card " <> show def <> " not found in set aside cards"

amongGathered :: (HasCallStack, ReverseQueue m) => CardMatcher -> ScenarioBuilderT m [Card]
amongGathered matcher = do
  x <- filterCards matcher . map toCard . unDeck <$> use (attrsL . encounterDeckL)
  y <- filterCards matcher <$> use otherCardsL
  pure $ x <> y

removeCards :: Monad m => [Card] -> ScenarioBuilderT m ()
removeCards xs = do
  attrsL . encounterDeckL %= filter ((`notElem` xs) . toCard)
  otherCardsL %= filter (`notElem` xs)

place :: ReverseQueue m => CardDef -> ScenarioBuilderT m LocationId
place def = do
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck [def]
  placeLocationCard def

placeLabeled_ :: ReverseQueue m => Text -> CardDef -> ScenarioBuilderT m ()
placeLabeled_ lbl def = void $ placeLabeled lbl def

placeLabeled :: ReverseQueue m => Text -> CardDef -> ScenarioBuilderT m LocationId
placeLabeled lbl def = do
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck [def]
  lid <- placeLocationCard def
  push $ SetLocationLabel lid lbl
  pure lid

placeInGrid :: ReverseQueue m => Pos -> CardDef -> ScenarioBuilderT m LocationId
placeInGrid pos def = do
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  otherCardsL %= deleteFirstMatch ((== def) . toCardDef)
  placeLocationCardInGrid pos def

placeInGrid_ :: ReverseQueue m => Pos -> CardDef -> ScenarioBuilderT m ()
placeInGrid_ pos def = do
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  void $ placeLocationCardInGrid pos def

place_ :: ReverseQueue m => CardDef -> ScenarioBuilderT m ()
place_ = void . place

placeAll :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
placeAll defs = do
  attrsL . encounterDeckL %= flip removeEachFromDeck defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck defs
  placeLocationCards defs

placeAllCapture :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m [LocationId]
placeAllCapture defs = traverse place defs

placeOneOf
  :: (SampleOneOf as, Sampled as ~ CardDef, ReverseQueue m) => as -> ScenarioBuilderT m LocationId
placeOneOf as = do
  def <- sampleOneOf as
  attrsL . encounterDeckL %= flip removeEachFromDeck (sampledFrom as)
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck (sampledFrom as)
  placeLocationCard def

placeOneOf_
  :: (SampleOneOf as, Sampled as ~ CardDef, ReverseQueue m) => as -> ScenarioBuilderT m ()
placeOneOf_ = void . placeOneOf

placeGroup :: ReverseQueue m => Text -> [CardDef] -> ScenarioBuilderT m ()
placeGroup groupName defs = do
  attrsL . encounterDeckL %= flip removeEachFromDeck defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck defs
  placeRandomLocationGroupCards groupName defs

placeGroupCapture :: ReverseQueue m => Text -> [CardDef] -> ScenarioBuilderT m [LocationId]
placeGroupCapture groupName defs = do
  attrsL . encounterDeckL %= flip removeEachFromDeck defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck defs
  placeRandomLocationGroupCardsCapture groupName defs

placeGroupChooseN :: ReverseQueue m => Int -> Text -> NonEmpty CardDef -> ScenarioBuilderT m ()
placeGroupChooseN n groupName = sampleN n >=> placeGroup groupName

startAt :: ReverseQueue m => LocationId -> ScenarioBuilderT m ()
startAt lid = do
  lead <- getLead
  attrs <- gets (.attrs)
  lift $ chooseOneM lead do
    targeting lid do
      reveal lid
      moveAllTo attrs lid

-- Does not handle extra encounter decks
addToEncounterDeck
  :: (ReverseQueue m, MonoFoldable defs, HasCardDef (Element defs)) => defs -> ScenarioBuilderT m ()
addToEncounterDeck (toList -> defs) = do
  cards <- traverse genEncounterCard defs
  attrsL . encounterDeckL %= withDeck (<> cards)

assetAt :: ReverseQueue m => CardDef -> LocationId -> ScenarioBuilderT m AssetId
assetAt def lid = do
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck [def]
  card <- genCard def
  createAssetAt card (AtLocation lid)

assetAt_ :: ReverseQueue m => CardDef -> LocationId -> ScenarioBuilderT m ()
assetAt_ def lid = void $ assetAt def lid

excludeFromEncounterDeck
  :: (ReverseQueue m, MonoFoldable defs, Element defs ~ card, HasCardDef card)
  => defs
  -> ScenarioBuilderT m ()
excludeFromEncounterDeck (toList -> cards) = do
  attrsL . encounterDeckL %= flip removeEachFromDeck (map toCardDef cards)

enemyAt_ :: ReverseQueue m => CardDef -> LocationId -> ScenarioBuilderT m ()
enemyAt_ def lid = do
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck [def]
  card <- genCard def
  createEnemyAt_ card lid

enemyAt :: ReverseQueue m => CardDef -> LocationId -> ScenarioBuilderT m EnemyId
enemyAt def lid = do
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck [def]
  card <- genCard def
  createEnemyAt card lid

placeEnemy
  :: (ReverseQueue m, IsPlacement placement) => CardDef -> placement -> ScenarioBuilderT m ()
placeEnemy def placement = do
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck [def]
  card <- genCard def
  pushM $ createEnemyWithPlacement_ card (toPlacement placement)

enemyAtMatching :: ReverseQueue m => CardDef -> LocationMatcher -> ScenarioBuilderT m ()
enemyAtMatching def matcher = do
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck [def]
  card <- genCard def
  createEnemyAtLocationMatching_ card matcher

sampleEncounterDeck :: (HasCallStack, MonadRandom m) => Int -> ScenarioBuilderT m [EncounterCard]
sampleEncounterDeck n = do
  deck <- use (attrsL . encounterDeckL)
  case nonEmpty (unDeck deck) of
    Nothing -> error "expected the encounter deck not to be empty"
    Just ne -> do
      cards <- sampleN n ne
      attrsL . encounterDeckL %= withDeck (filter (`notElem` cards))
      pure cards

class FindInEncounterDeck a where
  findInDeck :: a -> Deck EncounterCard -> Maybe EncounterCard
  notFoundInDeck :: ReverseQueue m => a -> m Card

instance FindInEncounterDeck CardDef where
  findInDeck def deck = find ((== def) . toCardDef) (unDeck deck)
  notFoundInDeck = genCard

instance FindInEncounterDeck Card where
  findInDeck card deck = find ((== card) . toCard) (unDeck deck)
  notFoundInDeck = pure

instance FindInEncounterDeck EncounterCard where
  findInDeck card deck = find (== card) (unDeck deck)
  notFoundInDeck = pure . toCard

-- Does not handle extra encounter decks
addExtraDeck
  :: (FindInEncounterDeck defs, ReverseQueue m) => ScenarioDeckKey -> [defs] -> ScenarioBuilderT m ()
addExtraDeck k defs = do
  deck <- use (attrsL . encounterDeckL)
  cards <- for defs \def -> do
    case findInDeck def deck of
      Just card -> do
        attrsL . encounterDeckL %= filter (/= card)
        pure $ toCard card
      Nothing -> notFoundInDeck def

  -- cards' <- shuffle cards
  attrsL . decksL %= (at k ?~ cards)

setActDeck :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
setActDeck defs = do
  cards <- genCards defs
  attrsL . actStackL %= insertMap 1 cards
  push SetActDeck

setAgendaDeck :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
setAgendaDeck defs = do
  cards <- genCards defs
  attrsL . agendaStackL %= insertMap 1 cards
  push SetAgendaDeck

setAgendaDeckN :: ReverseQueue m => Int -> [CardDef] -> ScenarioBuilderT m ()
setAgendaDeckN n defs = do
  cards <- genCards defs
  attrsL . agendaStackL %= insertMap n cards
  push SetAgendaDeck

setActDeckN :: ReverseQueue m => Int -> [CardDef] -> ScenarioBuilderT m ()
setActDeckN n defs = do
  cards <- genCards defs
  attrsL . actStackL %= insertMap n cards
  push SetActDeck

placeUnderScenarioReference :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
placeUnderScenarioReference defs = do
  cards <- genCards defs
  attrsL . encounterDeckL %= flip removeEachFromDeck defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck defs
  attrsL . cardsUnderScenarioReferenceL %= (<> cards)

beginWithStoryAsset :: ReverseQueue m => InvestigatorId -> CardDef -> ScenarioBuilderT m ()
beginWithStoryAsset iid def = do
  a <- genCard def
  attrsL . encounterDeckL %= flip removeEachFromDeck [def]
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck [def]
  push $ TakeControlOfSetAsideAsset iid a

class VictoryPlaceable a where
  toVictoryCard :: ReverseQueue m => a -> ScenarioBuilderT m Card

instance VictoryPlaceable CardDef where
  toVictoryCard = genCard

instance VictoryPlaceable Card where
  toVictoryCard = pure

-- NOTE: does not handle other encounter decks
placeInVictory
  :: (ReverseQueue m, VictoryPlaceable a, FindInEncounterDeck a) => [a] -> ScenarioBuilderT m ()
placeInVictory as = do
  cards <- traverse toVictoryCard as
  deck <- use (attrsL . encounterDeckL)
  for_ as \a -> do
    for_ (findInDeck a deck) \card -> attrsL . encounterDeckL %= filter (/= card)
  attrsL . victoryDisplayL %= (<> cards)

setLayout :: ReverseQueue m => [GridTemplateRow] -> ScenarioBuilderT m ()
setLayout = (attrsL . locationLayoutL .=)

setUsesGrid :: ReverseQueue m => ScenarioBuilderT m ()
setUsesGrid = attrsL . usesGridL .= True

setMeta :: (ReverseQueue m, ToJSON a) => a -> ScenarioBuilderT m ()
setMeta = (attrsL . metaL .=) . toJSON

setCount :: ReverseQueue m => ScenarioCountKey -> Int -> ScenarioBuilderT m ()
setCount k n = attrsL . countsL . at k . non 0 .= n

setExtraEncounterDeck
  :: (ReverseQueue m, FindInEncounterDeck a) => ScenarioEncounterDeckKey -> [a] -> ScenarioBuilderT m ()
setExtraEncounterDeck k as = do
  deck <- use (attrsL . encounterDeckL)
  cards <- for as \a -> do
    case (findInDeck a deck) of
      Just card -> do
        attrsL . encounterDeckL %= filter (/= card)
        pure $ toCard card
      Nothing -> notFoundInDeck a
  cards' <- shuffle cards
  attrsL . encounterDecksL . at k .= Just (Deck $ onlyEncounterCards cards', mempty)

pickN :: (HasCallStack, MonadRandom m) => Int -> [CardDef] -> ScenarioBuilderT m [CardDef]
pickN 0 defs = do
  attrsL . encounterDeckL %= flip removeEachFromDeck defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck defs
  pure []
pickN _ [] = pure []
pickN n (def : defs) = do
  (x, rest) <- sampleWithRest (def :| defs)
  (x :) <$> pickN (n - 1) rest

pickFrom
  :: (MonadRandom m, SampleOneOf as, Sampled as ~ CardDef)
  => as
  -> ScenarioBuilderT m CardDef
pickFrom defs = do
  attrsL . encounterDeckL %= flip removeEachFromDeck (sampledFrom defs)
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck (sampledFrom defs)
  sampleOneOf defs

placeTokensOnScenarioReference :: ReverseQueue m => Token -> Int -> ScenarioBuilderT m ()
placeTokensOnScenarioReference tokenType n = attrsL . tokensL %= addTokens tokenType n
