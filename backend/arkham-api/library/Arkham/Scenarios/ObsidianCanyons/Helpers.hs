module Arkham.Scenarios.ObsidianCanyons.Helpers where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Calculation
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Classes.HasQueue (HasQueue, push)
import Arkham.Classes.HasQueue qualified as Queue
import Arkham.Classes.Query
import Arkham.Deck qualified as Deck
import Arkham.Direction (GridDirection (..))
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Modifiers (modifySelect, modifySelectMapM)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario (countScenarioTokens, getScenarioDeck)
import Arkham.I18n
import Arkham.Id (BatchId, EnemyId, InvestigatorId, LocationId, getPlayer)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..), LocationAttrs)
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message (
  Message (PlaceEnemy, PlaceGrid, PlaceInvestigator, RemoveFromGame, Run, StoryMessage),
 )
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Message.Story (StoryMessage (PlaceStory, RemoveStory))
import Arkham.Modifier (ModifierType (..))
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Queue (QueueT)
import Arkham.Scenario.Deck (ScenarioDeckKey (SummitDeck))
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Tracing (Tracing)
import Arkham.Trait (Trait (Summit))

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "obsidianCanyons" a

{- | The storms over R'lyeh grow stronger as the scenario goes on. Setup places 1
resource on the scenario reference card under "Storm Intensity" and scenario
effects add and remove them, so the counter is the scenario's own resource token
count rather than anything held in the meta.
-}
getStormIntensity :: (HasGame m, Tracing m) => m Int
getStormIntensity = countScenarioTokens Token.Resource

{- | Marks an investigator who must draw the top card of the encounter deck at the
beginning of their first turn, from the "Let it in" branch of the /Dreams of
Destruction/ Task. It is carried by an @EffectTurnWindow@ effect created during the
intro, so it survives until that investigator's first turn ends and then expires on
its own — no bookkeeping needed to make it fire only once.
-}
dreamsOfDestruction :: ModifierType
dreamsOfDestruction = ScenarioModifier "dreamsOfDestruction"

{- | Lets an investigator walk out onto open sky, which is otherwise closed to
everyone. Primeval Terror grants it to whoever it is engaged with; the Open Sky
location reads it back so it does not have to know which cards can grant it.
-}
canEnterOpenSky :: ModifierType
canEnterOpenSky = ScenarioModifier "canEnterOpenSky"

{- | "Open sky" fills the gaps in the sky-city grid. It is a location for
adjacency and distance, so ordinary location matchers see it; anything that means
a real location has to say so.
-}
isOpenSky :: LocationMatcher
isOpenSky = locationIs Locations.openSky

notOpenSky :: LocationMatcher
notOpenSky = not_ isOpenSky

{- | The cards the winds can move: "each open sky and Summit location". The fixed
locations (R'lyeh Streets, Central Spire, and the other objectives) carry their
own traits rather than @Summit@, so they fall out by construction; the modifier
check is belt-and-braces for anything else that pins itself down.
-}
slidableLocation :: LocationMatcher
slidableLocation =
  oneOf [isOpenSky, LocationWithTrait Summit] <> not_ (LocationWithModifier CannotBeMoved)

{- | The placement diagrams number rows from the top down (row 1 is the top row),
while the engine's grid rows count upward from zero. Resolve a diagram row
against the grid as it currently stands; rows the grid does not have — the act 1
layout is only three rows tall, and the Winds refer to a fourth — give 'Nothing'.
-}
gridRowForDiagramRow :: (HasGame m, Tracing m) => Int -> m (Maybe Int)
gridRowForDiagramRow n = do
  rows <- map (.row) <$> getGridPositions
  pure do
    top <- maximumMay rows
    let row = top - (n - 1)
    guard (row `elem` rows)
    pure row

getGridPositions :: (HasGame m, Tracing m) => m [Pos]
getGridPositions = catMaybes <$> selectField LocationPosition (IncludeEmptySpace Anywhere)

-- | Everything occupying a grid row, ordered left to right by column.
getRowOccupants :: (HasGame m, Tracing m) => Int -> m [(Int, LocationId)]
getRowOccupants row = do
  lids <- select $ IncludeEmptySpace $ LocationInRow row
  cells <- for lids \lid -> fmap ((,lid) . (.column)) <$> field LocationPosition lid
  pure $ sortOn fst (catMaybes cells)

{- | The grid positions within @n@ spaces, counted as the scenario counts them:
"Adjacent locations share a side… Locations that share only a corner are not
considered adjacent", which makes distance the Manhattan distance on the grid.
Every cell of the layout is filled, so connection distance and step distance
agree.
-}
gridLocationsWithin
  :: (HasGame m, Tracing m) => Int -> LocationId -> m [LocationId]
gridLocationsWithin n lid = do
  field LocationPosition lid >>= \case
    Nothing -> pure []
    Just origin -> do
      others <- select $ IncludeEmptySpace $ not_ (LocationWithId lid)
      flip filterM others \other -> do
        field LocationPosition other <&> \case
          Nothing -> False
          Just pos -> gridDistance origin pos <= n

gridDistance :: Pos -> Pos -> Int
gridDistance a b = abs (a.column - b.column) + abs (a.row - b.row)

-- | The open sky cards sharing a side with the given location.
getAdjacentOpenSky :: (HasGame m, Tracing m) => LocationId -> m [LocationId]
getAdjacentOpenSky lid = do
  adjacent <- gridLocationsWithin 1 lid
  filterM (<=~> isOpenSky) adjacent

{- | Send locations out of play and onto the top of the Summit deck, "or place
them in the victory display instead if they have Victory X and no clues on them".
Open sky in particular must never reach the encounter discard pile.

@arrange@ orders the returning cards before they go on top; the winds shuffle
them, everything else keeps them as given.
-}
summitDeckCard :: (HasGame m, Tracing m) => LocationId -> m Card
summitDeckCard lid = do
  card <- field LocationCard lid
  revealed <- field LocationRevealed lid
  pure $ if revealed && not card.singleSided then flipCard card else card

returnToSummitDeckWith
  :: ReverseQueue m => ([Card] -> m [Card]) -> [LocationId] -> m ()
returnToSummitDeckWith arrange lids = do
  (toVictory, toDeck) <- partitionM canBeClaimedForVictory lids
  for_ toVictory addToVictory_
  returning <- arrange =<< traverse summitDeckCard toDeck
  for_ toDeck removeLocation
  deck <- getScenarioDeck SummitDeck
  setScenarioDeck SummitDeck (returning <> deck)

returnToSummitDeck :: ReverseQueue m => [LocationId] -> m ()
returnToSummitDeck = returnToSummitDeckWith pure

{- | "Locations can enter play in a position occupied by an open sky card. Should
this happen, the open sky card occupying that position leaves play" — and open
sky that leaves play goes back on top of the Summit deck.
-}
placeInOpenSky :: ReverseQueue m => Card -> LocationId -> m ()
placeInOpenSky card openSkyId = do
  field LocationPosition openSkyId >>= traverse_ \pos -> do
    returnToSummitDeck [openSkyId]
    placeLocationInGrid_ pos card

{- | Swap two cards' grid positions, carrying everything at them (they keep the
same @LocationId@, so @PlaceGrid@ preserves their contents). This is what both
"slide X into an adjacent open sky" and "swap X with the chosen open sky" do.
-}
swapGridPositions :: ReverseQueue m => LocationId -> LocationId -> m ()
swapGridPositions a b = do
  positions <- traverse (field LocationPosition) [a, b]
  case positions of
    [Just posA, Just posB] -> do
      push $ PlaceGrid (GridLocation posB a)
      push $ PlaceGrid (GridLocation posA b)
    _ -> pure ()

{- | "Cards are always drawn from the bottom of the Summit deck." Returns the
cards bottom-first and leaves the rest of the deck behind.
-}
drawFromSummitBottom :: ReverseQueue m => Int -> m [Card]
drawFromSummitBottom n = do
  deck <- getScenarioDeck SummitDeck
  let (remaining, bottom) = splitAt (max 0 (length deck - n)) deck
  setScenarioDeck SummitDeck remaining
  pure (reverse bottom)

{- | "Each location is connected to each location (and open sky) adjacent to it",
from the front of both agendas. Grid adjacency is not connection on its own, so
the agenda has to declare it.
-}
gridAdjacencyConnections :: (HasModifiersM m, Sourceable a) => a -> m ()
gridAdjacencyConnections a =
  modifySelectMapM a (IncludeEmptySpace Anywhere) \loc -> do
    field LocationPosition loc <&> \case
      Nothing -> []
      Just pos -> [ConnectedToWhen (LocationWithId loc) (mapOneOf LocationInPosition pos.adjacents)]

{- | One gust of the Eastern/Western Winds across the given diagram rows.

The edge-most slidable card of each row (leftmost when the wind blows left,
rightmost when it blows right) leaves play: onto the top of the Summit deck, or
into the victory display if it is worth victory points and holds no clues. The
rest of the row then slides one space, and the gap that opens at the far end is
refilled from the bottom of the Summit deck.
-}
blowWinds :: ReverseQueue m => [Int] -> GridDirection -> m ()
blowWinds diagramRows dir = do
  rows <- catMaybes <$> traverse gridRowForDiagramRow diagramRows
  plans <- traverse (planRowSlide dir) rows
  let gaps = mapMaybe (.gap) plans

  -- Plan the return and bottom draw as one deck update. Both setScenarioDeck and
  -- removals are queued, so calling returnToSummitDeckWith and then
  -- drawFromSummitBottom here would make the second helper read the old deck and
  -- overwrite the returned cards before they ever reached its top.
  let removed = mapMaybe (.removed) plans
  (toVictory, toDeck) <- partitionM canBeClaimedForVictory removed
  for_ toVictory addToVictory_
  returning <- shuffleM =<< traverse summitDeckCard toDeck
  for_ toDeck removeLocation

  deck <- getScenarioDeck SummitDeck
  let updatedDeck = returning <> deck
      (remaining, bottom) = splitAt (max 0 $ length updatedDeck - length gaps) updatedDeck
      fill = reverse bottom
  setScenarioDeck SummitDeck remaining

  for_ plans \plan ->
    for_ plan.slides \(pos, lid) -> push $ PlaceGrid (GridLocation pos lid)

  for_ (zip gaps fill) (uncurry placeLocationInGrid_)

{- | "or place them in the victory display instead if they have Victory X and no
clues on them"
-}
canBeClaimedForVictory :: (HasGame m, Tracing m) => LocationId -> m Bool
canBeClaimedForVictory lid = do
  victory <- field LocationVictory lid
  noClues <- lid <=~> LocationWithoutClues
  pure $ isJust victory && noClues

data RowSlide = RowSlide
  { removed :: Maybe LocationId
  , slides :: [(Pos, LocationId)]
  , gap :: Maybe Pos
  }

{- | Work out, without touching game state, what one row does when the wind blows
through it.

Immovable locations are skipped rather than shoved: the surviving slidable cards
are re-laid into the columns the slidable cards already occupy, in wind order,
which is exactly "moving the card that would enter its space into the nearest gap
created". One slidable card leaves, so one of those columns is always left over —
that is the gap.
-}
planRowSlide :: (HasGame m, Tracing m) => GridDirection -> Int -> m RowSlide
planRowSlide dir row = do
  occupants <- getRowOccupants row
  slidableIds <- map snd <$> filterM ((<=~> slidableLocation) . snd) occupants
  -- Read the row in the direction the wind blows, so "first" means the card the
  -- wind reaches first: the leftmost blowing left, the rightmost blowing right.
  let ordered = case dir of
        GridLeft -> occupants
        _ -> reverse occupants
      moving = [lid | (_, lid) <- ordered, lid `elem` slidableIds]
      openColumns = [c | (c, lid) <- ordered, lid `elem` slidableIds]
      staying = drop 1 moving
  pure
    $ RowSlide
      { removed = headMay moving
      , slides = [(Pos c row, lid) | (c, lid) <- zip openColumns staying]
      , gap = (`Pos` row) <$> headMay (drop (length staying) openColumns)
      }

-- | Shuffle cards into the Summit deck as a whole.
shuffleIntoSummitDeck :: ReverseQueue m => [Card] -> m ()
shuffleIntoSummitDeck cards = do
  deck <- getScenarioDeck SummitDeck
  setScenarioDeck SummitDeck =<< shuffleM (cards <> deck)

-- | "Shuffle … into the top N cards of the Summit deck."
shuffleIntoSummitTop :: ReverseQueue m => Int -> [Card] -> m ()
shuffleIntoSummitTop n cards = do
  deck <- getScenarioDeck SummitDeck
  let (top, rest) = splitAt n deck
  reshuffled <- shuffleM (cards <> top)
  setScenarioDeck SummitDeck (reshuffled <> rest)

{- | The act 2 placement diagram: 3 rows by 5 columns with Central Spire on the
left of the middle row.

> ? s ? ? s
> c ? s ? ?
> s ? ? ? s
-}
actTwoLayout :: SkylineLayout
actTwoLayout =
  mkSkylineLayout
    3
    [ "?s??s"
    , "c?s??"
    , "s???s"
    ]

{- | The act 3 placement diagram: 4 rows by 5 columns with Floating Spire in the
top left.

> f s ? s ?
> s ? ? ? ?
> ? s ? ? ?
> ? ? s ? ?
-}
actThreeLayout :: SkylineLayout
actThreeLayout =
  mkSkylineLayout
    4
    [ "fs?s?"
    , "s????"
    , "?s???"
    , "??s??"
    ]

data SkylineLayout = SkylineLayout
  { anchorPos :: Pos
  , openSkyPositions :: [Pos]
  , fillPositions :: [Pos]
  }

{- | Read a placement diagram. Rows are given top-down as the Campaign Guide
prints them, so the first string is the highest grid row; @?@ is filled from the
bottom of the Summit deck, @s@ takes a set-aside open sky card, and any other
character marks the anchor location that survives the rebuild.
-}
mkSkylineLayout :: Int -> [String] -> SkylineLayout
mkSkylineLayout rowCount rows =
  SkylineLayout
    { anchorPos = fromMaybe (Pos 0 0) (headMay [pos | (pos, c) <- cells, c /= '?', c /= 's'])
    , openSkyPositions = [pos | (pos, 's') <- cells]
    , fillPositions = [pos | (pos, '?') <- cells]
    }
 where
  cells =
    [ (Pos column (rowCount - 1 - rowIndex), c)
    | (rowIndex, row) <- zip [0 ..] rows
    , (column, c) <- zip [0 ..] row
    ]

{- | The skyline rebuild the act side-Bs share: everything in play except the
anchor goes back into the Summit deck (discarding whatever sat on it), the
set-aside open sky cards are laid out per the diagram, and every remaining space
is filled from the bottom of the deck.

The anchor keeps its own contents — it is the location the investigators are
standing on when the act advances.
-}
rebuildSkyline :: ReverseQueue m => LocationId -> SkylineLayout -> m ()
rebuildSkyline anchor layout = do
  -- "Shuffle each open sky and Summit location in play except for <anchor> into
  -- the Summit deck. Each card and token at those locations is discarded."
  -- The act removes R'lyeh Streets from the game first, so by this point
  -- everything on the grid other than the anchor is open sky or a Summit
  -- location, and "everything but the anchor" says it more safely than a trait
  -- check would (Central Spire and Floating Spire do not carry Summit on their
  -- revealed faces).
  sweeping <- select $ oneOf [isOpenSky, LocationWithTrait Summit]
  let leaving = filter (/= anchor) sweeping
  cards <- traverse summitDeckCard leaving
  for_ leaving removeLocation

  -- Build and draw from the new deck in one step. setScenarioDeck is queued, so
  -- calling shuffleIntoSummitDeck followed by drawFromSummitBottom here would
  -- make the draw see the old deck and leave holes in the new skyline.
  deck <- getScenarioDeck SummitDeck
  shuffled <- shuffleM (cards <> deck)
  let fillCount = length layout.fillPositions
  let (remaining, bottom) = splitAt (max 0 $ length shuffled - fillCount) shuffled
  setScenarioDeck SummitDeck remaining

  push $ PlaceGrid (GridLocation layout.anchorPos anchor)

  openSkies <- getSetAsideOpenSky (length layout.openSkyPositions)
  for_ (zip layout.openSkyPositions openSkies) (uncurry placeLocationInGrid_)

  let fill = reverse bottom
  for_ (zip layout.fillPositions fill) (uncurry placeLocationInGrid_)

{- | Take open sky cards from the set-aside pool. Setup puts every unused open sky
card there, so the acts just draw on it as their diagrams call for; placing a
location already removes its card from set aside.
-}
getSetAsideOpenSky :: (HasGame m, Tracing m) => Int -> m [Card]
getSetAsideOpenSky n = take n <$> select (SetAsideCardMatch $ cardIs Locations.openSky)

{- | "Place 1 doom on the nearest enemy." Ties are the investigator's choice, so
this asks when more than one enemy is equally near.
-}
placeDoomOnNearestEnemy
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
placeDoomOnNearestEnemy source iid = do
  nearest <- select $ NearestEnemyToFallback iid AnyEnemy
  chooseOrRunOneM iid $ targets nearest \eid -> placeDoom source eid 1

{- | The statics printed on the front of both agendas:

* "Each non-weakness enemy may enter or leave open sky as if it were a location."
  Open sky already /is/ a location here, so the permission is expressed as its
  complement — weaknesses are the ones held back. The agenda is in play for the
  whole scenario, so the two readings describe the same board.
* "Each location is connected to each location (and open sky) adjacent to it."
  Grid adjacency is not connection on its own, so this has to be declared.
-}
obsidianSkylineRules :: (HasModifiersM m, Sourceable a) => a -> m ()
obsidianSkylineRules a = do
  modifySelect a isOpenSky [CannotBeEnteredBy (not_ NonWeaknessEnemy)]
  gridAdjacencyConnections a

{- | "Remove <location> from the game, ignoring its text box." R'lyeh Streets and
Central Spire both say they cannot leave play, so the act has to go around the
usual leave-play path rather than through 'removeLocation'.
-}
removeIgnoringTextBox :: ReverseQueue m => LocationId -> m ()
removeIgnoringTextBox lid = push $ RemoveFromGame (LocationTarget lid)

{- | The @[action]@ every act in this scenario shares:

"Spend X clues: Reveal X cards from the bottom of the Summit deck. You may put 1
revealed location into play in an adjacent open sky and move to it. (Place that
open sky card and each other revealed card on top of the Summit deck in any
order.)"

The lead investigator chooses the order of every card returned to the top. Cards
are selected from bottom to top: each selected card is put on top immediately, so
the final selection becomes the deck's top card.
-}
chooseSummitTopOrder :: ReverseQueue m => [Card] -> m ()
chooseSummitTopOrder cards = when (notNull cards) do
  lead <- getLead
  player <- getPlayer lead
  (_, choices) <-
    runChooseT
      $ targets cards
      $ putCardOnTopOfDeck lead (Deck.ScenarioDeckByKey SummitDeck)
  let promptLabel = scenarioI18n $ "$" <> labelKey "searchTheSpires.chooseOrder"
  focusCards cards $ push $ Msg.chooseOrRunOneAtATimeWithLabel promptLabel player choices

searchTheSpires :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> Int -> m ()
searchTheSpires source iid x = when (x > 0) do
  revealed <- drawFromSummitBottom x
  openSkies <- maybe (pure []) getAdjacentOpenSky =<< selectOne (locationWithInvestigator iid)
  -- An open sky card revealed from the deck is a location card too, but there is
  -- nothing to "put into play" about swapping one patch of sky for another.
  let placeable = filter (not . (`cardMatch` cardIs Locations.openSky)) revealed
  focusCards revealed $ scenarioI18n do
    chooseOrRunOneM iid do
      questionLabeled' "searchTheSpires.chooseLocation"
      when (notNull openSkies) $ targets placeable \card -> do
        chooseTargetM iid openSkies \sky -> do
          skyCard <- summitDeckCard sky
          -- This replacement must not use placeInOpenSky: that helper puts the
          -- old sky on top immediately, before the lead can order it together
          -- with the other revealed cards.
          pos <- fieldJust LocationPosition sky
          removeLocation sky
          push $ Msg.ObtainCard card.id
          lid <- placeLocationInGrid pos card
          -- Return every unplaced card before moving. The move can satisfy the
          -- act's objective, so advancing first would strand these cards
          -- outside both play and the Summit deck.
          chooseSummitTopOrder (skyCard : filter (/= card) revealed)
          moveTo source iid lid
      labeled' "searchTheSpires.placeNone" $ chooseSummitTopOrder revealed

{- | The Forced printed on every Summit location's unrevealed back (and on Glyph
Orrery's front):

"When you would enter this location, if you do not control the Obsidian Claw: You
must either spend 1 clue or test [agility] (2). If you fail, cancel the effects of
the move."

Paired with 'summitEntryToll' in the location's @RunMessage@.
-}
summitEntry :: LocationAttrs -> Int -> Ability
summitEntry a n =
  skillTestAbility
    $ restricted a n (not_ $ youExist $ HasMatchingAsset controlsObsidianClaw)
    $ forced
    $ WouldMove #when You AnySource Anywhere (be a)

-- | Either face of the Claw counts; it can be flipped to (Power) mid-scenario.
controlsObsidianClaw :: AssetMatcher
controlsObsidianClaw = artifactInPlay Assets.obsidianClaw <> AssetControlledBy You

{- | Resolves 'summitEntry'. Spending the clue is only offered to investigators who
have one to spend; with no clue the test is the only way through.
-}
summitEntryToll :: ReverseQueue m => LocationAttrs -> Int -> InvestigatorId -> BatchId -> m ()
summitEntryToll a n iid batchId = do
  canSpendClue <- (> 0) <$> getSpendableClueCount [iid]
  chooseOrRunOneM iid $ scenarioI18n do
    when canSpendClue $ labeled' "summitEntry.spendClue" $ spendClues iid 1
    labeled' "summitEntry.test" do
      sid <- getRandom
      beginSkillTest sid iid (a.ability n) (BatchTarget batchId) #agility (Fixed 2)

{- | The failure half of 'summitEntryToll'. Kept separate because it hangs off
@FailedThisSkillTest@ rather than the ability itself.
-}
summitEntryFailed :: ReverseQueue m => LocationAttrs -> Int -> InvestigatorId -> m ()
summitEntryFailed a n iid = cancelMovement (a.ability n) iid

{- | "You may place those cards on either the top or bottom of the Summit deck, in
any order." Asked one card at a time; each answer settles that card's pile, and
the order within a pile follows the order they are resolved in.
-}
placeOnSummitTopOrBottom :: ReverseQueue m => InvestigatorId -> [Card] -> m ()
placeOnSummitTopOrBottom iid cards = focusCards cards $ chooseSummitPlacement iid cards

chooseSummitPlacement :: ReverseQueue m => InvestigatorId -> [Card] -> m ()
chooseSummitPlacement _ [] = pure ()
chooseSummitPlacement iid remaining = do
  highlightCards ([] :: [Card])
  chooseOneM iid $ targets remaining \card -> do
    highlightCards [card]
    chooseOneM iid $ scenarioI18n do
      labeled' "summitDeck.toTop" do
        putCardOnTopOfDeck iid (Deck.ScenarioDeckByKey SummitDeck) card
        chooseSummitPlacement iid $ filter (/= card) remaining
      labeled' "summitDeck.toBottom" do
        putCardOnBottomOfDeck iid (Deck.ScenarioDeckByKey SummitDeck) card
        chooseSummitPlacement iid $ filter (/= card) remaining

{- | Suspended Reef's "swap places with the chosen enemy, ignoring its location's
@Forced@ effect".

Both sides are /placed/ rather than moved. That is what discharges the "ignoring
its Forced effect" clause: the Summit entry toll hangs off @WouldMove@, and a
placement never opens that window. It also means the swap cannot be interrupted
half-way, leaving the enemy and investigator on the same square.
-}
swapPlacesWithEnemy :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
swapPlacesWithEnemy iid eid = do
  mFrom <- selectOne $ locationWithInvestigator iid
  mTo <- selectOne $ locationWithEnemy eid
  for_ ((,) <$> mFrom <*> mTo) \(here, there) -> do
    push $ PlaceInvestigator iid (AtLocation there)
    push $ PlaceEnemy eid (AtLocation here)

{- | Resolve whichever Winds card is in play "as if you had drawn a non-[elder
sign] symbol token" (Lost in the Clouds). The side in play decides the direction,
so this dispatches on it rather than duplicating the two stories' bodies.
-}
blowWindsFromStory :: ReverseQueue m => m ()
blowWindsFromStory = do
  eastern <- selectAny $ StoryIs (toCardCode Stories.easternWinds)
  if eastern
    then blowWinds [1, 3] GridLeft >> flipWinds Stories.easternWinds Stories.westernWinds
    else blowWinds [2, 4] GridRight >> flipWinds Stories.westernWinds Stories.easternWinds
 where
  flipWinds inPlay other = do
    selectEach (StoryIs (toCardCode inPlay)) (push . StoryMessage . RemoveStory)
    card <- genCard other
    push $ StoryMessage $ PlaceStory card Global

{- | "If a gap between locations and/or open sky is created for any reason and not
immediately filled via scenario card instructions, fill that gap with the bottom
card of the Summit deck."

A gap is an empty position enclosed by the grid the layout established, so this
looks only at holes inside the current bounding box — never off the edges, which
would grow the skyline every time something left play.
-}

{- | Whether the current effect already has grid placements queued after a
location removal. Winds, skyline rebuilds, and open-sky replacements all provide
their own replacement cards; the scenario's generic gap rule must not race ahead
of those instructions and fill the old gap before the cards slide.
-}
skylineInstructionsPending :: HasQueue Message m => QueueT Message m Bool
skylineInstructionsPending = lift $ Queue.fromQueue $ any hasGridPlacement
 where
  hasGridPlacement = \case
    PlaceGrid _ -> True
    Run messages -> any hasGridPlacement messages
    _ -> False

fillSkylineGaps :: ReverseQueue m => m ()
fillSkylineGaps = do
  positions <- getGridPositions
  unless (null positions) do
    let columns = map (.column) positions
        rows = map (.row) positions
        enclosed =
          [ Pos c r
          | c <- [minimumEx columns .. maximumEx columns]
          , r <- [minimumEx rows .. maximumEx rows]
          ]
    gaps <- filterM (selectNone . IncludeEmptySpace . LocationInPosition) enclosed
    fill <- drawFromSummitBottom (length gaps)
    for_ (zip gaps fill) (uncurry placeLocationInGrid_)

{- | "Open sky cannot be investigated and clues cannot be dropped on it. If a clue
would be dropped or placed on open sky for any reason, the investigators must
choose the nearest revealed location and place that clue on it instead."
-}
redirectCluesFromOpenSky :: ReverseQueue m => Int -> m ()
redirectCluesFromOpenSky n = when (n > 0) do
  lead <- getLead
  nearest <- select $ NearestLocationTo lead $ RevealedLocation <> notOpenSky
  chooseOrRunOneM lead $ targets nearest \target -> placeClues ScenarioSource target n
