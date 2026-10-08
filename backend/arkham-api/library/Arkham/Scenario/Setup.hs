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
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Helpers.Query (getLead)
import Arkham.Id
import Arkham.Key
import Arkham.Layout
import Arkham.Location.Grid
import Arkham.Location.Group
import Arkham.Matcher hiding (assetAt)
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Prelude hiding ((.=))
import Arkham.Scenario.Helpers (excludeDoubleSided, isDoubleSided)
import Arkham.Scenario.Runner (createEnemyWithPlacement, createEnemyWithPlacement_, pushM)
import Arkham.Scenario.Types
import Arkham.ScenarioLogKey
import Arkham.Target
import Arkham.Token (Token, addTokens)
import Control.Lens
import Control.Monad.Random (MonadRandom (..))
import Control.Monad.State.Strict
import Data.Function (on)
import Data.List (nubBy)
import Data.List.NonEmpty qualified as NE
import Data.Typeable

type SampledAs a b = (SampleOneOf a, Sampled a ~ b)

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
  , isReturnTo :: Bool
  , overrides :: SetupOverrides
  }

{- | Deltas a wrapping scenario declares before running an original setup block, so that
block can stay a verbatim copy of the one it wraps. An unofficial "Return to" box reads
like its printed scenario card: @replaceSet AgentsOfCthulhu StalkersOfCthulhu@,
@substitute Acts.thePit Acts.thePitV2@. Empty for every ordinary scenario, which is
every scenario that never calls one of these.
-}
data SetupOverrides = SetupOverrides
  { overriddenSets :: Map Set.EncounterSet (Maybe Set.EncounterSet)
  -- ^ 'Nothing' means the gather is skipped entirely.
  , overriddenCards :: Map CardCode CardDef
  , replacedCopies :: [CardDef]
  -- ^ Pending 'replaceOneOf' drops: one copy of each leaves the gathered pile, once.
  , cappedCopies :: [CardDef]
  -- ^ 'replaceOneOf' counterparts: the gathered pile keeps a single copy of each.
  , addedDeckCards :: Map ScenarioDeckKey [CardDef]
  -- ^ Pending 'alsoInDeck' additions, by the deck they join.
  , addedGridCards :: Map Text [(Pos, CardDef)]
  -- ^ Pending 'alsoPlaceInGrid' additions, by the 'placeShuffledInGrid' pool they join.
  , addedPoolCards :: Map Text [CardDef]
  -- ^ Pending 'addToPool' additions, by 'shuffledPool' key.
  , thinnedPools :: Map Text Int
  -- ^ Pending 'thinPool' removals, by 'shuffledPool' key.
  , excludedCards :: [CardDef]
  -- ^ 'excludeCards': defs that never reach play, however late they are gathered.
  }

noSetupOverrides :: SetupOverrides
noSetupOverrides = SetupOverrides mempty mempty mempty mempty mempty mempty mempty mempty mempty

overrideSetsL :: Lens' SetupOverrides (Map Set.EncounterSet (Maybe Set.EncounterSet))
overrideSetsL = lens (.overriddenSets) \m x -> m {overriddenSets = x}

overrideCardsL :: Lens' SetupOverrides (Map CardCode CardDef)
overrideCardsL = lens (.overriddenCards) \m x -> m {overriddenCards = x}

replacedCopiesL :: Lens' SetupOverrides [CardDef]
replacedCopiesL = lens (.replacedCopies) \m x -> m {replacedCopies = x}

cappedCopiesL :: Lens' SetupOverrides [CardDef]
cappedCopiesL = lens (.cappedCopies) \m x -> m {cappedCopies = x}

addedDeckCardsL :: Lens' SetupOverrides (Map ScenarioDeckKey [CardDef])
addedDeckCardsL = lens (.addedDeckCards) \m x -> m {addedDeckCards = x}

addedGridCardsL :: Lens' SetupOverrides (Map Text [(Pos, CardDef)])
addedGridCardsL = lens (.addedGridCards) \m x -> m {addedGridCards = x}

addedPoolCardsL :: Lens' SetupOverrides (Map Text [CardDef])
addedPoolCardsL = lens (.addedPoolCards) \m x -> m {addedPoolCards = x}

thinnedPoolsL :: Lens' SetupOverrides (Map Text Int)
thinnedPoolsL = lens (.thinnedPools) \m x -> m {thinnedPools = x}

excludedCardsL :: Lens' SetupOverrides [CardDef]
excludedCardsL = lens (.excludedCards) \m x -> m {excludedCards = x}

overridesL :: Lens' ScenarioBuilderState SetupOverrides
overridesL = lens (.overrides) \m x -> m {overrides = x}

-- | Gather @new@ wherever the wrapped block gathers @old@.
replaceSet :: Monad m => Set.EncounterSet -> Set.EncounterSet -> ScenarioBuilderT m ()
replaceSet old new = overridesL . overrideSetsL . at old .= Just (Just new)

-- | Skip the wrapped block's gather of this set.
ignoreSet :: Monad m => Set.EncounterSet -> ScenarioBuilderT m ()
ignoreSet old = overridesL . overrideSetsL . at old .= Just Nothing

{- | Use @new@ wherever the wrapped block names @old@ -- the "replace the X act card with
the new version from the Return set" instruction. A total substitution: for a partial
swap (one of each copy) write the cards out in your own block instead.
-}
substitute :: Monad m => CardDef -> CardDef -> ScenarioBuilderT m ()
substitute old new = do
  overridesL . overrideCardsL . at old.cardCode .= Just new
  attrsL . substitutionsL . at old.cardCode .= Just new.cardCode

{- | Use @new@ in place of ONE copy of @old@ -- the "replace one of each Tidal Pool with
its counterpart from the Return to set" instruction. Both sets are gathered, so this
drops a single copy of @old@ and keeps a single copy of @new@, however many the box
ships. A set with two copies of each therefore ends up with one of each.
-}
replaceOneOf :: Monad m => CardDef -> CardDef -> ScenarioBuilderT m ()
replaceOneOf old new = do
  overridesL . replacedCopiesL %= (old :)
  overridesL . cappedCopiesL %= (new :)

{- | Cards that never reach play, whichever set brings them and whenever it is gathered --
"remove two of the three at random without looking". Declared up front rather than read
off the pile, because a wrapping scenario runs before the block that gathers the original
set, so the pile is only half there when it declares.
-}
excludeCards :: Monad m => [CardDef] -> ScenarioBuilderT m ()
excludeCards defs = overridesL . excludedCardsL %= (<> defs)

{- | Apply the 'replaceOneOf' declarations to what has been gathered so far. Runs after
every gather, so a box may declare its swaps before or after the gathers that supply
them: the drops happen once each, and the caps are idempotent.
-}
applyReplacedCopies :: Monad m => ScenarioBuilderT m ()
applyReplacedCopies = do
  use (overridesL . replacedCopiesL) >>= filterM dropOne >>= (overridesL . replacedCopiesL .=)
  use (overridesL . cappedCopiesL) >>= traverse_ capToOne
  use (overridesL . excludedCardsL) >>= traverse_ dropEvery
 where
  -- 'True' keeps the declaration pending: no copy has been gathered yet.
  dropOne def = maybe (pure True) (\c -> removeGathered c >> pure False) =<< findGathered def
  dropEvery def = do
    cards <- gatheredCards
    traverse_ removeGathered [c | c <- cards, toCardDef c == def]
  capToOne def =
    findGathered def >>= \case
      Nothing -> pure ()
      Just kept -> do
        cards <- gatheredCards
        traverse_ removeGathered [c | c <- cards, toCardDef c == def, toCardId c /= toCardId kept]

  gatheredCards = do
    deck <- use (attrsL . encounterDeckL)
    others <- use otherCardsL
    pure (map toCard (unDeck deck) <> others)
  findGathered def = find ((== def) . toCardDef) <$> gatheredCards
  removeGathered card = do
    attrsL . encounterDeckL %= filter ((/= toCardId card) . toCardId)
    otherCardsL %= filter ((/= toCardId card) . toCardId)

{- | Cards that join a scenario deck the wrapped block builds with 'addExtraDeck', on top
of whatever it puts there. The deck is reshuffled so they do not all land on the bottom.
-}
alsoInDeck :: Monad m => ScenarioDeckKey -> [CardDef] -> ScenarioBuilderT m ()
alsoInDeck k defs = overridesL . addedDeckCardsL . at k . non [] %= (<> defs)

{- | A location, and the grid seat it takes, joining a pool the wrapped block places with
'placeShuffledInGrid' -- "shuffle Cave Mouth in with the rest; use all six".
-}
alsoPlaceInGrid :: Monad m => Text -> Pos -> CardDef -> ScenarioBuilderT m ()
alsoPlaceInGrid key pos def =
  overridesL . addedGridCardsL . at key . non [] %= (<> [(pos, def)])

-- | Cards that join a pool the wrapped block draws from with 'shuffledPool'.
addToPool :: Monad m => Text -> [CardDef] -> ScenarioBuilderT m ()
addToPool key defs = overridesL . addedPoolCardsL . at key . non [] %= (<> defs)

{- | Remove @n@ cards at random from a 'shuffledPool' without looking at them -- what a
box does after shuffling its own cards into one.
-}
thinPool :: Monad m => Text -> Int -> ScenarioBuilderT m ()
thinPool key n = overridesL . thinnedPoolsL . at key . non 0 %= (+ n)

{- | A pool of cards, shuffled, after any 'addToPool' and 'thinPool' a wrapping scenario
declared. The wrapped block names only its own cards and stays blind to the rest.
-}
shuffledPool :: MonadRandom m => Text -> [CardDef] -> ScenarioBuilderT m [CardDef]
shuffledPool key defs = do
  added <- use (overridesL . addedPoolCardsL . at key . non [])
  dropped <- use (overridesL . thinnedPoolsL . at key . non 0)
  drop dropped <$> shuffleM (defs <> added)

{- | Shuffle a pool of locations across the given grid seats, including any seat a
wrapping scenario added with 'alsoPlaceInGrid'.
-}
placeShuffledInGrid :: ReverseQueue m => Text -> [Pos] -> [CardDef] -> ScenarioBuilderT m ()
placeShuffledInGrid key seats defs = do
  added <- use (overridesL . addedGridCardsL . at key . non [])
  zipWithM_ placeInGrid (seats <> map fst added) =<< shuffleM (defs <> map snd added)

-- | Resolve a gather through 'replaceSet' or 'ignoreSet'.
resolveSet
  :: Monad m => Set.EncounterSet -> ScenarioBuilderT m (Maybe Set.EncounterSet)
resolveSet s = use (overridesL . overrideSetsL . at s) <&> fromMaybe (Just s)

-- | Resolve a card def through 'substitute'.
resolveDef :: Monad m => CardDef -> ScenarioBuilderT m CardDef
resolveDef def = use (overridesL . overrideCardsL . at def.cardCode) <&> fromMaybe def

resolveDefs :: Monad m => [CardDef] -> ScenarioBuilderT m [CardDef]
resolveDefs = traverse resolveDef

attrsL :: Lens' ScenarioBuilderState ScenarioAttrs
attrsL = lens (.attrs) \m x -> m {attrs = x}

otherCardsL :: Lens' ScenarioBuilderState [Card]
otherCardsL = lens (.otherCards) \m x -> m {otherCards = x}

isReturnToL :: Lens' ScenarioBuilderState Bool
isReturnToL = lens (.isReturnTo) \m x -> m {isReturnTo = x}

newtype ScenarioBuilderT m a = ScenarioBuilderT {unScenarioBuilderT :: StateT ScenarioBuilderState m a}
  deriving newtype
    (Functor, Applicative, Monad, MonadIO, MonadState ScenarioBuilderState, MonadTrans)

instance MonadRandom m => MonadRandom (ScenarioBuilderT m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

{- | Card generation is deliberately NOT routed through 'substitute': 'gather' mints the
cards of the set it is given, and resolving there would make the box's stand-in appear
twice -- once in place of the original and once from the box's own set. Substitution
belongs where a def is named, which the helpers below do explicitly.
-}
instance CardGen m => CardGen (ScenarioBuilderT m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cid = lift . replaceCard cid
  removeCard = lift . removeCard
  clearCardCache = lift clearCardCache

instance HasQueue Message m => HasQueue Message (ScenarioBuilderT m) where
  messageQueue = lift messageQueue
  pushAll = lift . pushAll

instance HasGame m => HasGame (ScenarioBuilderT m) where
  getGame = lift getGame
  getCache = GameCache \_ build -> build

instance ReverseQueue m => ReverseQueue (ScenarioBuilderT m) where
  filterInbox = lift . filterInbox

runScenarioSetup
  :: (MonadRandom m, HasGame m)
  => (ScenarioAttrs -> b)
  -> ScenarioAttrs
  -> ScenarioBuilderT m ()
  -> m b
runScenarioSetup f attrs body =
  f
    . (.attrs)
    <$> execStateT
      (clearCards >> body.unScenarioBuilderT >> shuffleEncounterDeck)
      (ScenarioBuilderState (attrs & campaignStepL .~ Nothing) [] False noSetupOverrides)

shuffleEncounterDeck :: (HasGame m, MonadRandom m, MonadState ScenarioBuilderState m) => m ()
shuffleEncounterDeck = do
  mods <- getModifiers ScenarioTarget
  let extraCards = nubBy ((==) `on` toCardId) [card | StartsInEncounterDeck card <- mods]
  encounterDeck <- removeNonEncounterBackLocationCards <$> use (attrsL . encounterDeckL)
  shuffledEncounterDeck <- withDeckM shuffleM (Deck $ unDeck encounterDeck <> extraCards)
  attrsL . encounterDeckL .= shuffledEncounterDeck
 where
  removeNonEncounterBackLocationCards = filter (not . isNonEncounterBackLocationCard)
  isNonEncounterBackLocationCard = and . sequence [(== LocationType) . cdCardType, cdDoubleSided] . toCardDef

clearCards :: MonadState ScenarioBuilderState m => m ()
clearCards = do
  attrsL . encounterDeckL .= Deck []
  attrsL . discardL .= []
  attrsL . victoryDisplayL .= []

gather :: CardGen m => Set.EncounterSet -> ScenarioBuilderT m ()
gather = withResolvedSet \encounterSet -> do
  (other, cards) <- partition isDoubleSided <$> gatherEncounterSet encounterSet
  attrsL . encounterDeckL %= (Deck cards <>)
  otherCardsL %= (map toCard other <>)
  applyReplacedCopies

{- | Run a gather against the set 'replaceSet' names in its place, or not at all if
'ignoreSet' dropped it. Plain for every scenario that declares no overrides.
-}
withResolvedSet
  :: Monad m
  => (Set.EncounterSet -> ScenarioBuilderT m ())
  -> Set.EncounterSet
  -> ScenarioBuilderT m ()
withResolvedSet f encounterSet = resolveSet encounterSet >>= traverse_ f

gatherJust :: CardGen m => Set.EncounterSet -> [CardDef] -> ScenarioBuilderT m ()
gatherJust encounterSet defs = withResolvedSet go encounterSet
 where
  go s = do
    cards <-
      filter ((`cardMatch` mapOneOf cardDefIs defs) . toCard)
        . excludeDoubleSided
        <$> gatherEncounterSet s
    attrsL . encounterDeckL %= (Deck cards <>)

gatherJustMatching :: ReverseQueue m => Set.EncounterSet -> CardMatcher -> ScenarioBuilderT m ()
gatherJustMatching encounterSet matcher = withResolvedSet go encounterSet
 where
  go s = do
    gather s
    removeCards =<< amongGathered (CardFromEncounterSet s <> not_ matcher)

gatherAndSetAside :: ReverseQueue m => Set.EncounterSet -> ScenarioBuilderT m ()
gatherAndSetAside = withResolvedSet \encounterSet -> do
  cards <- map toCard <$> gatherEncounterSet encounterSet
  push $ SetAsideCards cards

gatherOneOf
  :: (SampleOneOf as, Sampled as ~ Set.EncounterSet, CardGen m) => as -> ScenarioBuilderT m ()
gatherOneOf = sampleOneOf >=> gather

setAsideKeys :: ReverseQueue m => [ArkhamKey] -> ScenarioBuilderT m ()
setAsideKeys ks = attrsL . setAsideKeysL %= (<> setFromList ks)

setAsideEvery :: ReverseQueue m => CardMatcher -> ScenarioBuilderT m ()
setAsideEvery matcher = do
  cards <- fromGathered matcher
  attrsL . setAsideCardsL %= (<> cards)

placeStory :: ReverseQueue m => CardDef -> ScenarioBuilderT m ()
placeStory def = do
  card <- genCard def
  removeEvery [def]
  push $ StoryMessage $ PlaceStory card Global

placeStoryCapture :: ReverseQueue m => CardDef -> ScenarioBuilderT m StoryId
placeStoryCapture def = do
  placeStory def
  pure $ StoryId def.cardCode

setAside :: (ReverseQueue m, FindInEncounterDeck a, HasCallStack) => [a] -> ScenarioBuilderT m ()
setAside = setAsideWith pure

setAsideFacedown
  :: (ReverseQueue m, FindInEncounterDeck a, HasCallStack) => [a] -> ScenarioBuilderT m ()
setAsideFacedown = setAsideWith (setFacedown True)

setAsideWith
  :: (ReverseQueue m, FindInEncounterDeck a, HasCallStack)
  => (Card -> ScenarioBuilderT m Card) -> [a] -> ScenarioBuilderT m ()
setAsideWith f as0 = do
  as <- traverse resolveFindable as0
  cards <- for as \a -> do
    deck <- use (attrsL . encounterDeckL)
    case findInDeck a deck of
      Just card -> do
        attrsL . encounterDeckL %= filter (/= card)
        otherCardsL %= filter (/= toCard card)
        pure $ toCard card
      Nothing -> do
        card <- notFoundInDeck a
        otherCardsL %= filter (/= toCard card)

        for_ (cdOtherSide $ toCardDef card) \otherSide -> do
          otherCardsL %= filter ((`notElem` [otherSide, toCardCode card]) . toCardCode)

        pure card

  cards' <- traverse f cards
  attrsL . setAsideCardsL %= (<> cards')
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
removeOneOf def = removeOneOfEach def.defs

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

fromGathered :: (HasCallStack, ReverseQueue m) => CardMatcher -> ScenarioBuilderT m [Card]
fromGathered matcher = do
  cards <- amongGathered matcher
  removeCards cards
  pure cards

fromGathered1 :: (HasCallStack, ReverseQueue m) => CardDef -> ScenarioBuilderT m Card
fromGathered1 def = do
  amongGathered (cardDefIs def) >>= \case
    [card] -> do
      removeCards [card]
      pure card
    [] ->
      amongGathered (mapOneOf cardIs $ def : maybeToList def.flip) >>= \case
        [card] -> do
          removeCards [card]
          let otherSide = flipCard card
          replaceCard card.id otherSide
          pure otherSide
        xs ->
          error
            $ unlines
              [ "expected exactly one matching card in gathered cards: "
              , show def
              , show xs
              , prettyCallStack callStack
              ]
    xs ->
      error
        $ unlines
          [ "expected exactly one matching card in gathered cards: "
          , show def
          , show xs
          , prettyCallStack callStack
          ]

removeCards :: Monad m => [Card] -> ScenarioBuilderT m ()
removeCards xs = do
  attrsL . encounterDeckL %= filter ((`notElem` xs) . toCard)
  otherCardsL %= filter (`notElem` xs)

doNotShuffleIn :: Monad m => [Card] -> ScenarioBuilderT m ()
doNotShuffleIn = removeCards

place :: ReverseQueue m => CardDef -> ScenarioBuilderT m LocationId
place def = do
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck def.defs
  placeLocationCard def

placeLabeled_ :: ReverseQueue m => Text -> CardDef -> ScenarioBuilderT m ()
placeLabeled_ lbl def = void $ placeLabeled lbl def

placeLabeled :: ReverseQueue m => Text -> CardDef -> ScenarioBuilderT m LocationId
placeLabeled lbl def = do
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck def.defs
  lid <- placeLocationCard def
  push $ SetLocationLabel lid lbl
  pure lid

{- | Declare the groups whose boxes the map draws. Membership comes from 'placeLocationGroup'
(or 'Arkham.Helpers.Location.joinLocationGroup' for a location that arrives later); this
only fixes each group's key and how its box lays its members out.
-}
setLocationGroups :: Monad m => [LocationGroup] -> ScenarioBuilderT m ()
setLocationGroups groups = attrsL . locationGroupsL .= groups

{- | Place a whole group at once, in the order given. The index is assigned here and
stored on each location, so the order inside the box is fixed at placement time and
survives reload, undo and replay — a later query returning members in a different order
cannot reshuffle the box.

Members may also carry a grid position; a group only changes how they are drawn and how
connections are routed to them.
-}
placeLocationGroup
  :: ReverseQueue m => LocationGroupKey -> [CardDef] -> ScenarioBuilderT m [LocationId]
placeLocationGroup key defs = for (zip [0 ..] defs) \(i, def) -> do
  lid <- place def
  push $ SetLocationGroup lid (GroupMembership key i)
  pure lid

placeLocationGroup_ :: ReverseQueue m => LocationGroupKey -> [CardDef] -> ScenarioBuilderT m ()
placeLocationGroup_ key defs = void $ placeLocationGroup key defs

placeInGrid :: ReverseQueue m => Pos -> CardDef -> ScenarioBuilderT m LocationId
placeInGrid pos def = do
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  otherCardsL %= deleteFirstMatch ((== def) . toCardDef)
  placeLocationCardInGrid pos def

placeInGrid_ :: ReverseQueue m => Pos -> CardDef -> ScenarioBuilderT m ()
placeInGrid_ pos def = void $ placeInGrid pos def

placeCardInGrid :: ReverseQueue m => Pos -> Card -> ScenarioBuilderT m LocationId
placeCardInGrid pos card = do
  attrsL . encounterDeckL %= flip removeEachFromDeck card.defs
  otherCardsL %= deleteFirstMatch (== card)
  placeLocationInGrid pos card

placeCardInGrid_ :: ReverseQueue m => Pos -> Card -> ScenarioBuilderT m ()
placeCardInGrid_ pos card = void $ placeCardInGrid pos card

place_ :: ReverseQueue m => CardDef -> ScenarioBuilderT m ()
place_ = void . place

placeAll :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
placeAll defs = do
  attrsL . encounterDeckL %= flip removeEachFromDeck defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck defs
  placeLocationCards defs

placeAllCapture :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m [LocationId]
placeAllCapture defs = traverse place defs

placeOneOf :: (SampledAs as CardDef, ReverseQueue m) => as -> ScenarioBuilderT m LocationId
placeOneOf as = do
  def <- sampleOneOf as
  attrsL . encounterDeckL %= flip removeEachFromDeck (sampledFrom as)
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck (sampledFrom as)
  placeLocationCard def

placeOneOf_ :: (SampledAs as CardDef, ReverseQueue m) => as -> ScenarioBuilderT m ()
placeOneOf_ = void . placeOneOf

placeGroup :: ReverseQueue m => Text -> [CardDef] -> ScenarioBuilderT m ()
placeGroup groupName defs = do
  attrsL . encounterDeckL %= flip removeEachFromDeck defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck defs
  placeRandomLocationGroupCards groupName defs

placeGroupExact :: ReverseQueue m => Text -> [CardDef] -> ScenarioBuilderT m ()
placeGroupExact groupName defs = do
  attrsL . encounterDeckL %= flip removeEachFromDeck defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck defs
  placeLabeledLocations_ groupName =<< genCards defs

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
  lift $ chooseOneM lead do
    targeting lid do
      reveal lid
      placeAllAt lid

-- Does not handle extra encounter decks
addToEncounterDeck
  :: (ReverseQueue m, MonoFoldable defs, HasCardDef (Element defs)) => defs -> ScenarioBuilderT m ()
addToEncounterDeck (toList -> defs) = do
  cards <- traverse genEncounterCard defs
  attrsL . encounterDeckL %= withDeck (<> cards)

assetAt :: ReverseQueue m => CardDef -> LocationId -> ScenarioBuilderT m AssetId
assetAt def0 lid = do
  def <- resolveDef def0
  -- Both the named card and its 'substitute' leave the decks: neither is left to be drawn.
  let defs = nub (def0.defs <> def.defs)
  attrsL . encounterDeckL %= flip removeEachFromDeck defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck defs
  card <- genCard def
  createAssetAt card (AtLocation lid)

assetAt_ :: ReverseQueue m => CardDef -> LocationId -> ScenarioBuilderT m ()
assetAt_ def lid = void $ assetAt def lid

placeAsset_ :: ReverseQueue m => CardDef -> Placement -> ScenarioBuilderT m ()
placeAsset_ def p = void $ placeAsset def p

placeAsset :: ReverseQueue m => CardDef -> Placement -> ScenarioBuilderT m AssetId
placeAsset def p = do
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck def.defs
  card <- genCard def
  createAssetAt card p

excludeFromEncounterDeck
  :: (ReverseQueue m, MonoFoldable defs, Element defs ~ card, HasCardDef card)
  => defs
  -> ScenarioBuilderT m ()
excludeFromEncounterDeck (toList -> cards) = do
  attrsL . encounterDeckL %= flip removeEachFromDeck (map toCardDef cards)

enemyAt_ :: ReverseQueue m => CardDef -> LocationId -> ScenarioBuilderT m ()
enemyAt_ def lid = do
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck def.defs
  card <- genCard def
  createEnemyAt_ card lid

enemyAt :: ReverseQueue m => CardDef -> LocationId -> ScenarioBuilderT m EnemyId
enemyAt def lid = do
  mcard <- headMay <$> amongGathered (cardDefIs def)
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck def.defs
  card <- maybe (genCard def) pure mcard
  createEnemyAt card lid

placeEnemy
  :: (ReverseQueue m, IsPlacement placement) => CardDef -> placement -> ScenarioBuilderT m ()
placeEnemy def placement = do
  mcard <- headMay <$> amongGathered (cardDefIs def)
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck def.defs
  card <- maybe (genCard def) pure mcard
  pushM $ createEnemyWithPlacement_ card (toPlacement placement)

placeEnemyCapture
  :: (ReverseQueue m, IsPlacement placement) => CardDef -> placement -> ScenarioBuilderT m EnemyId
placeEnemyCapture def placement = do
  mcard <- headMay <$> amongGathered (cardDefIs def)
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck def.defs
  card <- maybe (genCard def) pure mcard
  (enemyId, msg) <- createEnemyWithPlacement card (toPlacement placement)
  push msg
  pure enemyId

enemyAtMatching :: ReverseQueue m => CardDef -> LocationMatcher -> ScenarioBuilderT m ()
enemyAtMatching def matcher = do
  mcard <- headMay <$> amongGathered (cardDefIs def)
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck def.defs
  card <- maybe (genCard def) pure mcard
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

  -- | Send what the wrapped block named through 'substitute'. Only a def can be swapped.
  resolveFindable :: Monad m => a -> ScenarioBuilderT m a
  resolveFindable = pure

instance FindInEncounterDeck CardDef where
  findInDeck def deck = find ((== def) . toCardDef) (unDeck deck)
  notFoundInDeck = genCard
  resolveFindable = resolveDef

instance FindInEncounterDeck Card where
  findInDeck card deck = find ((== card) . toCard) (unDeck deck)
  notFoundInDeck = pure

instance FindInEncounterDeck EncounterCard where
  findInDeck card deck = find (== card) (unDeck deck)
  notFoundInDeck = pure . toCard

-- Does not handle extra encounter decks
addExtraDeck
  :: (FindInEncounterDeck defs, ReverseQueue m) => ScenarioDeckKey -> [defs] -> ScenarioBuilderT m ()
addExtraDeck k defs0 = do
  defs <- traverse resolveFindable defs0
  cards <- traverse fromDeckOrGen defs
  added <- use (overridesL . addedDeckCardsL . at k . non [])
  -- 'alsoInDeck' cards are shuffled through the deck rather than stacked under it.
  deck <- case added of
    [] -> pure cards
    _ -> shuffleM . (cards <>) =<< traverse fromDeckOrGen added
  attrsL . decksL %= (at k ?~ deck)

fromDeckOrGen
  :: (FindInEncounterDeck a, ReverseQueue m) => a -> ScenarioBuilderT m Card
fromDeckOrGen def = do
  deck <- use (attrsL . encounterDeckL)
  case findInDeck def deck of
    Just card -> do
      attrsL . encounterDeckL %= filter (/= card)
      pure $ toCard card
    Nothing -> notFoundInDeck def

addAdditionalReferences :: ReverseQueue m => [CardCode] -> ScenarioBuilderT m ()
addAdditionalReferences codes = attrsL . additionalReferencesL %= (<> codes)

setActDeck :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
setActDeck defs = do
  cards <- genCards =<< resolveDefs defs
  attrsL . actStackL %= insertMap 1 cards
  push SetActDeck

setAgendaDeck :: ReverseQueue m => [CardDef] -> ScenarioBuilderT m ()
setAgendaDeck defs = do
  cards <- genCards =<< resolveDefs defs
  attrsL . agendaStackL %= insertMap 1 cards
  push SetAgendaDeck

setAgendaDeckN :: ReverseQueue m => Int -> [CardDef] -> ScenarioBuilderT m ()
setAgendaDeckN n defs = do
  cards <- genCards =<< resolveDefs defs
  attrsL . agendaStackL %= insertMap n cards
  push SetAgendaDeck

setActDeckN :: ReverseQueue m => Int -> [CardDef] -> ScenarioBuilderT m ()
setActDeckN n defs = do
  cards <- genCards =<< resolveDefs defs
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
  attrsL . encounterDeckL %= flip removeEachFromDeck def.defs
  attrsL . encounterDecksL . each . _1 %= flip removeEachFromDeck def.defs
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

setIsReturnTo :: ReverseQueue m => ScenarioBuilderT m ()
setIsReturnTo = isReturnToL .= True

whenReturnTo :: ReverseQueue m => ScenarioBuilderT m () -> ScenarioBuilderT m ()
whenReturnTo a = do
  isReturnTo' <- use isReturnToL
  when isReturnTo' a

orWhenReturnTo
  :: ReverseQueue m => ScenarioBuilderT m a -> ScenarioBuilderT m a -> ScenarioBuilderT m a
orWhenReturnTo b a = do
  isReturnTo' <- use isReturnToL
  if isReturnTo' then a else b

orDoReturnTo :: ReverseQueue m => a -> ScenarioBuilderT m a -> ScenarioBuilderT m a
orDoReturnTo b a = do
  isReturnTo' <- use isReturnToL
  if isReturnTo' then a else pure b

orSampleIfReturnTo
  :: forall a m. (Eq a, Typeable a, ReverseQueue m) => a -> [a] -> ScenarioBuilderT m a
orSampleIfReturnTo b as =
  b `orDoReturnTo` do
    (a, rest) <- sampleWithRest (b :| as)
    case eqT @a @CardDef of
      Just Refl -> removeEvery rest
      Nothing -> pure ()
    pure a

getIsReturnTo :: ReverseQueue m => ScenarioBuilderT m Bool
getIsReturnTo = use isReturnToL
setMeta :: (ReverseQueue m, ToJSON a) => a -> ScenarioBuilderT m ()
setMeta = (attrsL . metaL .=) . toJSON

setCount :: ReverseQueue m => ScenarioCountKey -> Int -> ScenarioBuilderT m ()
setCount k n = attrsL . countsL . at k . non 0 .= n

setExtraEncounterDeck
  :: (ReverseQueue m, FindInEncounterDeck a) => ScenarioEncounterDeckKey -> [a] -> ScenarioBuilderT m ()
setExtraEncounterDeck k as = do
  deck <- use (attrsL . encounterDeckL)
  cards <- for as \a -> do
    case findInDeck a deck of
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

cardDefIs :: HasCardDef a => a -> CardMatcher
cardDefIs a = if def.doubleSided then cardIs def else cardIsExact def
 where
  def = toCardDef a
