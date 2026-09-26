module AH3e.Game where

import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
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
  , performed :: [ActionKind]
  , bonusActions :: Int
  , lockedAssets :: [CardId]
  , usedAssets :: [CardId]
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
  , setAside :: [CardId]
  , removed :: [CardId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyDecks :: Decks
emptyDecks = Decks mempty [] [] [] mempty [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] []

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
  , damagePrevented :: Int
  {- ^ Damage a prevention test just prevented, waiting for the harm it was
  cast against to pick it up (416.6).
  -}
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
  , debug :: Bool
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
    , performed = []
    , bonusActions = 0
    , lockedAssets = []
    , usedAssets = []
    }
