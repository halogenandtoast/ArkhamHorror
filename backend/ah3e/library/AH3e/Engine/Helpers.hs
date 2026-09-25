module AH3e.Engine.Helpers where

import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

label :: Text -> [Message] -> Choice
label t = Choice (TextLabel t)

removeCardEverywhere :: CardId -> GameM ()
removeCardEverywhere cid = do
  let del = filter (/= cid)
  #decks %= \d ->
    d
      { neighborhoods = Map.map del d.neighborhoods
      , street = del d.street
      , travelRoute = del d.travelRoute
      , threshold = del d.threshold
      , mysteries = Map.map del d.mysteries
      , event = del d.event
      , eventDiscard = del d.eventDiscard
      , monster = del d.monster
      , headline = del d.headline
      , headlineDiscard = del d.headlineDiscard
      , item = del d.item
      , ally = del d.ally
      , spell = del d.spell
      , display = del d.display
      , anomaly = del d.anomaly
      , terror = del d.terror
      , special = del d.special
      , starting = del d.starting
      , conditions = del d.conditions
      , archive = del d.archive
      , setAside = del d.setAside
      , removed = del d.removed
      }

-- 430.7, 412.3: shuffle a card together with the top two cards of a deck
shuffleIntoTopTwo :: CardId -> [CardId] -> GameM [CardId]
shuffleIntoTopTwo cid deck = do
  let (top, rest) = splitAt 2 deck
  top' <- shuffle (cid : top)
  pure (top' <> rest)

drawBottom :: [a] -> Maybe (a, [a])
drawBottom [] = Nothing
drawBottom xs = Just (last xs, init xs)

evalAmount :: EffectCtx -> Amount -> Int
evalAmount ctx = \case
  N n -> n
  TestResult -> fromMaybe 0 ctx.testResult
  Half a -> let n = evalAmount ctx a in (n + 1) `div` 2
  Diff a b -> max 0 (evalAmount ctx a - evalAmount ctx b)
  Counted _ -> error "counted amounts are fixed before evaluation"

addRemnants :: InvestigatorId -> Int -> GameM ()
addRemnants iid n = investigatorL iid . #remnants += n

addMoney :: InvestigatorId -> Int -> GameM ()
addMoney iid n = investigatorL iid . #money %= max 0 . (+ n)

addClues :: InvestigatorId -> Int -> GameM ()
addClues iid n = investigatorL iid . #clues %= max 0 . (+ n)

-- 467.5, 467.6, 451.2, 425.2: which investigators a monster engages when it shares their space
engageTargets
  :: CardId -> [Investigator] -> Maybe InvestigatorRule -> GameM (Either [Investigator] [Investigator])
engageTargets mid present mPrey = do
  d <- monsterDef mid
  if
    | null present -> pure (Right [])
    | Elusive `elem` d.keywords -> pure (Right [])
    | Massive `elem` d.keywords -> pure (Right present)
    | otherwise -> do
        prey <- case mPrey of
          Nothing -> pure []
          Just rule -> ruleInvestigators rule
        let preyHere = [i | i <- present, i.id `elem` map (.id) prey]
            pool = if null preyHere then present else preyHere
        pure $ case pool of
          [i] -> Right [i]
          _ -> Left pool

activationPrey :: CardId -> GameM (Maybe InvestigatorRule)
activationPrey mid =
  monsterDef mid <&> \d -> case d.activation of
    Hunter r -> Just r
    Patrol _ r -> r
    _ -> Nothing

isMonsterReady :: CardId -> GameM Bool
isMonsterReady mid = uses #monsters (maybe False ((== Ready) . (.state)) . Map.lookup mid)

setMonsterState :: CardId -> MonsterState -> GameM ()
setMonsterState mid st = monsterL mid . #state .= st

-- 428.11: a new engagement replaces the old one (except massive)
engage :: InvestigatorId -> CardId -> GameM ()
engage iid mid = do
  d <- monsterDef mid
  m <- getMonster mid
  sid <- fromMaybe m.space <$> investigatorSpace iid
  let st
        | Massive `elem` d.keywords = case m.state of
            Engaged is -> Engaged (if iid `elem` is then is else is <> [iid])
            _ -> Engaged [iid]
        | otherwise = Engaged [iid]
  monsterL mid . #state .= st
  monsterL mid . #space .= sid

-- 455.3: ready monsters in the space engage the entering investigator
engageOnEntry :: InvestigatorId -> SpaceId -> GameM Bool
engageOnEntry iid sid = do
  ms <- monstersAt sid
  engaging <- fmap catMaybes $ for ms \m -> do
    d <- monsterDef m.card
    let massive = Massive `elem` d.keywords
        elusive = Elusive `elem` d.keywords
        eligible = case m.state of
          Ready -> not elusive
          Engaged is -> massive && iid `notElem` is
          Exhausted -> False
    pure $ if eligible then Just m.card else Nothing
  for_ engaging (engage iid)
  pure (not (null engaging))

moveEngagedWatchers :: InvestigatorId -> SpaceId -> GameM ()
moveEngagedWatchers iid sid = do
  ms <- engagedMonsters iid
  for_ ms \m -> monsterL m.card . #space .= sid

spaceChoices :: [SpaceId] -> (SpaceId -> [Message]) -> [Choice]
spaceChoices sids f = [Choice (SpaceLabel s) (f s) | s <- sids]

eventDef :: CardId -> GameM EventDef
eventDef cid =
  getCardDef cid <&> \d -> case d.kind of
    EventCard e -> e
    _ -> error ("not an event card " <> show cid)

allNeighborhoodSpaces :: GameM [SpaceId]
allNeighborhoodSpaces = uses (#board . #spaces) (map (.id) . filter (isNeighborhoodSpace . (.kind)) . Map.elems)

assetDeckLens :: AssetDeckKind -> Lens' Game [CardId]
assetDeckLens = \case
  ItemDeckKind -> #decks . #item
  AllyDeckKind -> #decks . #ally
  SpellDeckKind -> #decks . #spell

cardValue :: CardId -> GameM (Maybe Int)
cardValue cid = (>>= (.value)) <$> assetDef cid

itemMatches :: Maybe Trait -> Maybe ValueBound -> CardId -> GameM Bool
itemMatches mtrait mbound cid = do
  traitOk <- maybe (pure True) (\t -> cardMatches (WithTrait t) cid) mtrait
  v <- cardValue cid
  let valueOk = case mbound of
        Nothing -> True
        Just (AtMost n) -> maybe False (<= n) v
        Just (AtLeast n) -> maybe False (>= n) v
  pure (traitOk && valueOk)

newTest :: InvestigatorId -> Skill -> Int -> TestKind -> AfterTest -> TestState
newTest iid skill modifier kind after =
  TestState
    { investigator = iid
    , skill = skill
    , modifier = modifier
    , kind = kind
    , step = DeterminePool
    , bonusDice = 0
    , chosenAssets = []
    , dice = []
    , addedSuccesses = 0
    , after = after
    , casting = Nothing
    }

-- 491.3b: reveal from the bottom of the monster deck until the trait is found
revealMonstersFromBottom :: Trait -> Int -> GameM [CardId]
revealMonstersFromBottom trait n = go n [] []
 where
  go 0 found revealed = finish found revealed
  go k found revealed = do
    deck <- use (#decks . #monster)
    case drawBottom deck of
      Nothing -> finish found revealed
      Just (cid, rest) -> do
        #decks . #monster .= rest
        d <- monsterDef cid
        if trait `elem` d.traits
          then go (k - 1) (found <> [cid]) revealed
          else go k found (cid : revealed)
  finish found revealed = do
    shuffled <- shuffle revealed
    #decks . #monster %= (shuffled <>)
    pure found

-- spaces of the given kind closest to a space by investigator movement
nearestSpacesMatching :: (SpaceKind -> Bool) -> SpaceId -> GameM [SpaceId]
nearestSpacesMatching p from = do
  board <- use #board
  let dist = distancesFrom (`adjacentSpaces` board) from
      scored = [(s.id, d) | s <- Map.elems board.spaces, p s.kind, Just d <- [Map.lookup s.id dist]]
  pure $ case scored of
    [] -> []
    _ -> let best = minimum (map snd scored) in [sid | (sid, d) <- scored, d == best]

markersAt :: SpaceId -> GameM [Marker]
markersAt sid = (.markers) <$> getSpace sid

allMarkers :: GameM [(SpaceId, Marker)]
allMarkers = uses (#board . #spaces) \spaces -> [(s.id, m) | s <- Map.elems spaces, m <- s.markers]

markerSpace :: Text -> GameM (Maybe SpaceId)
markerSpace color = listToMaybe . map fst . filter ((== color) . (.color) . snd) <$> allMarkers
