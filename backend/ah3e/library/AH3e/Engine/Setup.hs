module AH3e.Engine.Setup (
  GameMode (..),
  GameOptions (..),
  defaultOptions,
  newGame,
  availableScenarios,
  setupScenario,
) where

import AH3e.Content
import AH3e.Engine.Monad
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Ids
import AH3e.Types.State
import Data.List (partition)
import Data.Map.Strict qualified as Map

data GameOptions = GameOptions
  { expansions :: [Expansion]
  , mode :: GameMode
  , debug :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultOptions :: GameOptions
defaultOptions = GameOptions [CoreSet] StandardMode False

emptyGame :: [PlayerId] -> Int -> GameOptions -> Game
emptyGame pids seed opts =
  Game
    { scenario = Nothing
    , mode = opts.mode
    , seed = seed
    , expansions = opts.expansions
    , nextCardId = 1
    , status = InProgress
    , phase = SetupPhase
    , round = 0
    , players = [PlayerState pid Nothing | pid <- pids]
    , leader = fromJustNote "no players" (listToMaybe pids)
    , investigators = mempty
    , usedInvestigators = []
    , startingInvestigatorCount = length pids
    , board = emptyBoard
    , cards = mempty
    , monsters = mempty
    , assets = mempty
    , decks = emptyDecks
    , codex = []
    , sheetDoom = 0
    , sheetClues = 0
    , cup = []
    , drawnTokens = []
    , turn = Nothing
    , activatedMonsters = []
    , terrorEncountered = []
    , test = Nothing
    , pendingSuccesses = 0
    , damagePrevented = 0
    , encounter = Nothing
    , revealedEvent = Nothing
    , activeCard = Nothing
    , activeToken = Nothing
    , questionsAsked = 0
    , phasesEntered = []
    , queue = []
    , questions = mempty
    , log = []
    , rumor = Nothing
    , rumorIgnored = []
    , debug = opts.debug
    }

newGame :: [PlayerId] -> Int -> GameOptions -> Either Text Game
newGame pids seed opts = do
  when (null pids) $ Left "A game needs at least one player"
  when (length pids > 6) $ Left "Arkham Horror supports one to six players"
  when (null (availableScenarios opts.expansions))
    $ Left "No playable scenario in the chosen expansions"
  pure (emptyGame pids seed opts) {queue = [ChooseScenario]}

availableScenarios :: [Expansion] -> [ScenarioDef]
availableScenarios expansions = [sc | sc <- Map.elems scenarioDefs, sc.expansion `elem` expansions]

-- rules 102-109
setupScenario :: ScenarioDef -> GameM ()
setupScenario sc = do
  expansions <- use #expansions
  mode <- use #mode
  let defs = [d | d <- Map.elems cardDefs, d.expansion `elem` expansions]
      copiesOf d = replicateM (max 1 d.copies) (newCard d.code)
  -- 102
  #board .= buildBoard sc.setupMap
  board <- use #board
  let onBoard nid = Map.member nid board.neighborhoods
      kinds = map (.kind) (Map.elems board.spaces)
      hasKind p = any p kinds
  for_ defs \d -> case d.kind of
    NeighborhoodCard nid _ | onBoard nid -> do
      cids <- copiesOf d
      #decks . #neighborhoods . at nid %= Just . (<> cids) . fromMaybe []
    StreetCard _ -> copiesOf d >>= \cids -> #decks . #street %= (<> cids)
    TravelRouteCard _ | hasKind isRoute -> copiesOf d >>= \cids -> #decks . #travelRoute %= (<> cids)
    ThresholdCard _ | hasKind isThreshold -> copiesOf d >>= \cids -> #decks . #threshold %= (<> cids)
    MysteryCard m | Map.member m.space board.spaces -> do
      cids <- copiesOf d
      #decks . #mysteries . at m.space %= Just . (<> cids) . fromMaybe []
    HeadlineCard _ -> copiesOf d >>= \cids -> #decks . #headline %= (<> cids)
    AssetCard a -> do
      cids <- copiesOf d
      case a.origin of
        AllyDeck -> #decks . #ally %= (<> cids)
        ItemDeck -> #decks . #item %= (<> cids)
        SpellDeck -> #decks . #spell %= (<> cids)
        SpecialPile -> #decks . #special %= (<> cids)
        StartingPile -> #decks . #starting %= (<> cids)
        ConditionPile -> #decks . #conditions %= (<> cids)
        Archive -> #decks . #archive %= (<> cids)
    ConditionCard _ -> copiesOf d >>= \cids -> #decks . #conditions %= (<> cids)
    ArchiveCard _ -> copiesOf d >>= \cids -> #decks . #archive %= (<> cids)
    AnomalyCard _ -> copiesOf d >>= \cids -> #decks . #setAside %= (<> cids)
    TerrorCard _ -> copiesOf d >>= \cids -> #decks . #setAside %= (<> cids)
    _ -> pure ()
  #decks . #neighborhoods <~ (use (#decks . #neighborhoods) >>= traverse shuffle)
  #decks . #mysteries <~ (use (#decks . #mysteries) >>= traverse shuffle)
  #decks . #street <~ (use (#decks . #street) >>= shuffle)
  #decks . #travelRoute <~ (use (#decks . #travelRoute) >>= shuffle)
  #decks . #threshold <~ (use (#decks . #threshold) >>= shuffle)
  -- 103
  events <- for sc.eventCards newCard
  #decks . #event <~ shuffle events
  -- 104
  let (known, unknown) = partition (isJust . cardDef . fst) sc.monsters
  for_ unknown \(mcode, _) -> logText ("Monster card data missing: " <> coerce mcode)
  monsters <- fmap concat $ for known \(mcode, n) -> replicateM n (newCard mcode)
  placed <- placeStartingMonsters monsters sc.startingMonsters
  #decks . #monster <~ shuffle (filter (`notElem` placed) monsters)
  -- 105
  #cup .= adjustCup mode (concat [replicate n t | (t, n) <- sc.mythosCup])
  -- 106
  headlines <- use (#decks . #headline)
  shuffled <- shuffle headlines
  #decks . #headline .= take 13 shuffled
  -- 107
  #decks . #item <~ (use (#decks . #item) >>= shuffle)
  #decks . #ally <~ (use (#decks . #ally) >>= shuffle)
  #decks . #spell <~ (use (#decks . #spell) >>= shuffle)
  refill
  push AskInvestigatorChoice
 where
  isRoute = \case TravelRouteSpace _ -> True; _ -> False
  isThreshold = \case ThresholdSpace _ -> True; _ -> False
  refill = do
    deck <- use (#decks . #item)
    #decks . #display .= take 5 deck
    #decks . #item .= drop 5 deck

placeStartingMonsters :: [CardId] -> [(CardCode, SpaceId)] -> GameM [CardId]
placeStartingMonsters pool = go []
 where
  go used [] = pure used
  go used ((mcode, sid) : rest) = do
    candidates <- filterM (\cid -> (== mcode) <$> cardCode cid) (filter (`notElem` used) pool)
    case candidates of
      (cid : _) -> do
        #monsters . at cid ?= Monster {card = cid, space = sid, state = Ready, damage = 0}
        go (cid : used) rest
      [] -> go used rest

adjustCup :: GameMode -> [MythosToken] -> [MythosToken]
adjustCup mode cup = case mode of
  StandardMode -> cup
  StoryMode -> replaceOne SpreadDoomToken BlankToken cup
  ChallengeMode -> replaceOne BlankToken SpreadDoomToken cup
 where
  replaceOne from to' = \case
    [] -> []
    (x : xs) | x == from -> to' : xs
    (x : xs) -> x : replaceOne from to' xs

buildBoard :: MapDef -> Board
buildBoard m =
  Board
    { spaces = Map.fromList [(s.id, toSpace s) | s <- allSpaces]
    , neighborhoods =
        Map.fromList
          [ ( n.id
            , Neighborhood
                { id = n.id
                , name = n.name
                , town = n.town
                , spaces = map (.id) n.spaces
                , clues = 0
                , anomaly = False
                , terror = 0
                , attachedTerror = []
                , markers = []
                }
            )
          | n <- m.neighborhoods
          ]
    , borders =
        Map.fromListWith
          (<>)
          (concat [[(a, Map.singleton b h), (b, Map.singleton a h)] | (a, b, h) <- m.borders])
    , layout = m.layout
    }
 where
  allSpaces :: [SpaceDef]
  allSpaces = concatMap (\n -> [s & #neighborhood ?~ n.id | s <- n.spaces]) m.neighborhoods <> m.otherSpaces
  toSpace :: SpaceDef -> Space
  toSpace s =
    Space
      { id = s.id
      , name = s.name
      , kind = s.kind
      , neighborhood = s.neighborhood
      , doom = 0
      , clues = 0
      , markers = []
      }
