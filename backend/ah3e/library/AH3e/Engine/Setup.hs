module AH3e.Engine.Setup (
  GameMode (..),
  GameOptions (..),
  defaultOptions,
  newGame,
  availableScenarios,
  setupScenario,
  buildBoard,
) where

import AH3e.Content
import AH3e.Content.Tiles (spaceIdFor)
import AH3e.Engine.Monad
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Ids
import AH3e.Types.State
import Data.List (nub, partition)
import Data.Map.Strict qualified as Map

data GameOptions = GameOptions
  { expansions :: [Expansion]
  , mode :: GameMode
  , scenario :: Maybe ScenarioCode
  -- ^ the scenario the table was made for; without one the group is asked
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultOptions :: GameOptions
defaultOptions = GameOptions [] StandardMode Nothing

{- | The boxes a table is playing with. The core box is always one of them: every
other box is an addition to it, and leans on its cards for everything from the
street deck to the epic monsters a codex card calls up.
-}
expansionsOf :: GameOptions -> [Expansion]
expansionsOf opts = nub (CoreSet : opts.expansions)

emptyGame :: [PlayerId] -> Int -> GameOptions -> Game
emptyGame pids seed opts =
  Game
    { scenario = Nothing
    , mode = opts.mode
    , seed = seed
    , expansions = expansionsOf opts
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
    , sheetMarkers = 0
    , sheetTokens = mempty
    , bystanders = Nothing
    , unstableSpace = Nothing
    , cup = []
    , drawnTokens = []
    , turn = Nothing
    , activatedMonsters = []
    , terrorEncountered = []
    , test = Nothing
    , suspendedTests = []
    , provoked = mempty
    , pendingSuccesses = 0
    , pendingRiders = Nothing
    , damagePrevented = 0
    , horrorPrevented = 0
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
    }

newGame :: [PlayerId] -> Int -> GameOptions -> Either Text Game
newGame pids seed opts = do
  when (null pids) $ Left "A game needs at least one player"
  when (length pids > 6) $ Left "Arkham Horror supports one to six players"
  let playable = availableScenarios (expansionsOf opts)
  when (null playable) $ Left "No playable scenario in the chosen expansions"
  start <- case opts.scenario of
    Nothing -> pure ChooseScenario
    Just code -> do
      unless (code `elem` map (.code) playable) $ Left "That scenario is not in the chosen expansions"
      pure (SelectScenario code)
  pure (emptyGame pids seed opts) {queue = [start]}

{- | "When a threshold tile is added to the map, orient the hazardous borders
randomly" (Secrets of the Order, p. 4). The tile's icons are laid out in order when
the board is built, so turning it is a matter of dealing them round its borders again.
-}
turnThresholdTiles :: GameM ()
turnThresholdTiles = do
  board <- use #board
  let isThresholdSpace sid = case Map.lookup sid board.spaces of
        Just s -> case s.kind of ThresholdSpace _ -> True; _ -> False
        Nothing -> False
  for_ [s.id | s <- Map.elems board.spaces, isThresholdSpace s.id] \sid -> do
    edges <- uses (#board . #borders . at sid . non mempty) Map.toList
    turned <- shuffle (mapMaybe snd edges)
    let dealt = zip (map fst edges) (map Just turned <> repeat Nothing)
    for_ dealt \(other, h) -> do
      #board . #borders . ix sid . at other ?= h
      #board . #borders . ix other . at sid ?= h

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
  for_ [d | d <- defs, d.code `notElem` sc.setAside] \d -> case d.kind of
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
  {- Set-aside cards are filtered the same way as the monster pool: a sheet that
  says "set aside all Lodge monsters" names them from every box. -}
  aside <-
    fmap concat $ for [d | d <- mapMaybe cardDef sc.setAside, d.expansion `elem` expansions] copiesOf
  #decks . #setAside %= (<> aside)
  for_ sc.startingMarkers \(sid, colour) ->
    #board . #spaces . ix sid . #markers %= (<> [Marker colour True])
  turnThresholdTiles
  -- 103
  events <- for sc.eventCards newCard
  #decks . #event <~ shuffle events
  -- 104
  {- A sheet that asks for a whole trait ("every Deep One monster") names the
  monsters from every box, so the ones whose expansion is not in play are left out
  rather than treated as missing. -}
  let (found, unknown) = partition (isJust . cardDef . fst) sc.monsters
      known = [m | m@(mcode, _) <- found, maybe False ((`elem` expansions) . (.expansion)) (cardDef mcode)]
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

{- | The monsters a sheet puts on the board before play. A shrouded monster is named
by the face it shows while ready, which several cards share, and one of them is taken
at random so the side it will turn over is a surprise (Secrets of the Order, p. 6).
-}
placeStartingMonsters :: [CardId] -> [(CardCode, SpaceId)] -> GameM [CardId]
placeStartingMonsters pool = go []
 where
  go used [] = pure used
  go used ((mcode, sid) : rest) = do
    candidates <- filterM (named mcode) (filter (`notElem` used) pool)
    chosen <- pickRandom candidates
    case maybeToList chosen of
      (cid : _) -> do
        #monsters
          . at cid
          ?= Monster {card = cid, space = sid, state = Ready, damage = 0, markers = [], prey = Nothing}
        go (cid : used) rest
      [] -> go used rest
  {- A shrouded monster's own code is the engaged face nobody has looked at yet, so a
  sheet names it by the face it shows while ready. -}
  named wanted cid = do
    code <- cardCode cid
    if code == wanted
      then pure True
      else do
        d <- monsterDef' cid
        pure (any ((== coerce wanted) . coerce . spaceIdFor) d.readyName)
  monsterDef' cid =
    getCardDef' cid <&> \d -> case d.kind of
      MonsterCard m -> m
      _ -> error ("not a monster " <> show cid)
  getCardDef' cid = do
    code <- cardCode cid
    pure (fromJustNote ("no card def for " <> show code) (cardDef code))

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
