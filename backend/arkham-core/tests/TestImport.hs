module TestImport
  ( module X
  , newGame
  )
where

import Arkham.Types.Classes as X
import Arkham.Types.Game as X
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.Message as X
import Arkham.Types.Phase
import ClassyPrelude as X
import Control.Monad.Fail as X
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID as UUID
import Lens.Micro as X
import Test.Hspec as X

newGame :: Investigator -> IORef [Message] -> Game (IORef [Message])
newGame investigator queue = Game
  { gameMessages = queue
  , gameSeed = 1
  , gameCampaign = Nothing
  , gameScenario = Nothing
  , gamePlayerCount = 1
  , gameLocations = mempty
  , gameEnemies = mempty
  , gameAssets = mempty
  , gameInvestigators = HashMap.singleton investigatorId investigator
  , gamePlayers = HashMap.singleton 1 investigatorId
  , gameActiveInvestigatorId = investigatorId
  , gameLeadInvestigatorId = investigatorId
  , gamePhase = CampaignPhase -- TODO: maybe this should be a TestPhase or something?
  , gameEncounterDeck = mempty
  , gameDiscard = mempty
  , gameSkillTest = Nothing
  , gameAgendas = mempty
  , gameTreacheries = mempty
  , gameEvents = mempty
  , gameSkills = mempty
  , gameActs = mempty
  , gameChaosBag = Bag []
  , gameGameOver = False
  , gamePending = False
  , gameUsedAbilities = mempty
  , gameFocusedCards = mempty
  , gameFocusedTokens = mempty
  , gameActiveCard = Nothing
  , gamePlayerOrder = [investigatorId]
  , gameVictoryDisplay = mempty
  , gameQuestion = mempty
  , gameHash = UUID.nil
  }
  where investigatorId = getInvestigatorId investigator
