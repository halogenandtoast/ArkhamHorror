module AH3e.Game where

import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State

data Investigator = Investigator
  { id :: InvestigatorId
  , player :: PlayerId
  , status :: InvestigatorStatus
  , space :: Maybe SpaceId
  , damage :: Int
  , horror :: Int
  , money :: Int
  , clues :: Int
  , remnants :: Int
  , focus :: Map Skill Int
  , delayed :: Bool
  , active :: Bool
  , assets :: [CardId]
  , actionsTaken :: Int
  , spacesMoved :: Int
  -- ^ how far the move action in flight has carried them, for cards that count it
  , spacesMovedThisRound :: Int
  {- ^ how far they have been carried since the round began, which a sheet may
  count across several moves (Stella Clark's delivery route)
  -}
  , performed :: [ActionKind]
  , bonusActions :: Int
  , lockedAssets :: [CardId]
  , usedAssets :: [CardId]
  , usedAbilities :: [Text]
  -- ^ once-a-round abilities of the investigator's own, spent this round
  , lastTestDice :: Maybe Int
  {- ^ how many dice they last rolled, for a card that matches somebody else's
  pool rather than working out its own (Anything You Can Do). Left optional, as
  'fixedPoolNext' is, so a table saved before either existed still loads.
  -}
  , fixedPoolNext :: Maybe Int
  {- ^ a pool their next test rolls in place of working one out, left by a card
  that states it outright ("instead of your normal dice pool")
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InvestigatorStatus = Joining | Playing | Defeated | Devoured | Retired
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Monster = Monster
  { card :: CardId
  , space :: SpaceId
  , state :: MonsterState
  , damage :: Int
  , markers :: [Marker]
  -- ^ markers a scenario has put on the monster itself, which travel with it
  , prey :: Maybe InvestigatorId
  {- ^ whoever a card has named as this monster's prey for the monster phase,
  in place of the rule its activation prints (Silas Marsh)
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Asset = Asset
  { card :: CardId
  , owner :: InvestigatorId
  , damage :: Int
  , horror :: Int
  , flipped :: Bool
  , attachedTo :: Maybe CardId
  , tokens :: Map Text Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Rumor = Rumor {card :: CardId, doom :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CodexEntry = CodexEntry
  { number :: ArchiveNumber
  , card :: CardId
  , flipped :: Bool
  , tokens :: Map Text Int
  , fired :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Decks = Decks
  { neighborhoods :: Map NeighborhoodId [CardId]
  , street :: [CardId]
  , travelRoute :: [CardId]
  , threshold :: [CardId]
  , mysteries :: Map SpaceId [CardId]
  , event :: [CardId]
  , eventDiscard :: [CardId]
  , monster :: [CardId]
  , headline :: [CardId]
  , headlineDiscard :: [CardId]
  , item :: [CardId]
  , ally :: [CardId]
  , spell :: [CardId]
  , display :: [CardId]
  , anomaly :: [CardId]
  , terror :: [CardId]
  , special :: [CardId]
  , starting :: [CardId]
  , conditions :: [CardId]
  , archive :: [CardId]
  , investigation :: [ArchiveNumber]
  -- ^ the archive cards a scenario is still choosing between (Dreams of R'lyeh)
  , investigationUnder :: Maybe ArchiveNumber
  -- ^ the codex card that pile lies under, so the table can show its depth there
  , setAside :: [CardId]
  , removed :: [CardId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyDecks :: Decks
emptyDecks = Decks mempty [] [] [] mempty [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] Nothing [] []

data PlayerState = PlayerState
  { id :: PlayerId
  , investigator :: Maybe InvestigatorId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Game = Game
  { scenario :: Maybe ScenarioCode
  , mode :: GameMode
  , seed :: Int
  , expansions :: [Expansion]
  , nextCardId :: Int
  , status :: GameStatus
  , phase :: Phase
  , round :: Int
  , players :: [PlayerState]
  , leader :: PlayerId
  , investigators :: Map InvestigatorId Investigator
  , usedInvestigators :: [InvestigatorId]
  , startingInvestigatorCount :: Int
  , board :: Board
  , cards :: Map CardId CardCode
  , monsters :: Map CardId Monster
  , assets :: Map CardId Asset
  , decks :: Decks
  , codex :: [CodexEntry]
  , sheetDoom :: Int
  , sheetClues :: Int
  , sheetMarkers :: Int
  -- ^ markers a scenario sheet has collected, which some cards count (429.7)
  , sheetTokens :: Map Text Int
  {- ^ the scenario sheet's other piles, by name: a sheet may collect damage and
  horror tokens as well as markers, and a scenario may keep its own state here.
  -}
  , bystanders :: Maybe [(CardId, SpaceId)]
  {- ^ Ally cards lying facedown on the board, which The Dead Cry Out calls
  bystanders: monsters hunt them, and whoever reaches one first may take the card.
  Optional, so a table saved before it loads.
  -}
  , unstableSpace :: Maybe SpaceId
  {- ^ The space a card has made the unstable space in place of the one the
  event deck names (Desperate Binding moves it to the Witch House). Optional, so
  a table saved before it loads.
  -}
  , cup :: [MythosToken]
  , drawnTokens :: [MythosToken]
  , turn :: Maybe InvestigatorId
  , activatedMonsters :: [CardId]
  , terrorEncountered :: [InvestigatorId]
  , test :: Maybe TestState
  , suspendedTests :: [TestState]
  {- ^ Tests waiting for the one in 'test' to finish, innermost first. A card that
  tests while another test resolves (Wither, Intervene) stacks one on another, and
  the view still shows whichever is being resolved now.
  -}
  , provoked :: Map CardId [InvestigatorId]
  {- ^ Who has attacked or damaged each monster. A monster that would otherwise
  pass an investigator by (Tattered Cloak) still engages the ones who provoked it.
  -}
  , pendingSuccesses :: Int
  {- ^ Successes a card promised before its test began -- a spell's cast cost
  lands before the casting test does -- picked up by the next test to start.
  -}
  , pendingRiders :: Maybe [(EffectCtx, Effect)]
  {- ^ Riders a card left before its test existed, the way 'pendingSuccesses'
  banks successes (Book of Shadows arms itself as the cast is paid for). Picked
  up by the next test to start. Optional, so a table saved before it loads.
  -}
  , damagePrevented :: Int
  {- ^ Damage a prevention test just prevented, waiting for the harm it was
  cast against to pick it up (416.6).
  -}
  , horrorPrevented :: Int
  -- ^ likewise for horror, which a card may prevent without a test
  , encounter :: Maybe EncounterState
  , revealedEvent :: Maybe CardId
  , activeCard :: Maybe CardId
  {- ^ The card currently in front of the players: an encounter, a headline
  being read, or an event the mythos just turned up.
  -}
  , activeToken :: Maybe MythosToken
  -- ^ The mythos token just drawn, shown until its continue prompt is answered.
  , questionsAsked :: Int
  {- ^ How many questions have been asked all game, so an effect can notice
  that it resolved without needing any input.
  -}
  , -- \^ The last event card turned face up, including mythos draws nobody acts on.
    phasesEntered :: [Phase]
  -- ^ Phases begun since the last answer, in order, for announcing them.
  , queue :: [Message]
  , questions :: Map PlayerId Question
  , log :: [Text]
  , rumor :: Maybe Rumor
  , rumorIgnored :: [InvestigatorId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newInvestigator :: InvestigatorId -> PlayerId -> Investigator
newInvestigator iid pid =
  Investigator
    { id = iid
    , player = pid
    , status = Joining
    , space = Nothing
    , damage = 0
    , horror = 0
    , money = 0
    , clues = 0
    , remnants = 0
    , focus = mempty
    , delayed = False
    , active = True
    , assets = []
    , actionsTaken = 0
    , spacesMoved = 0
    , spacesMovedThisRound = 0
    , performed = []
    , bonusActions = 0
    , lockedAssets = []
    , usedAssets = []
    , usedAbilities = []
    , lastTestDice = Nothing
    , fixedPoolNext = Nothing
    }
