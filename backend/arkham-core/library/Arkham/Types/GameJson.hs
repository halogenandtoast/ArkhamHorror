module Arkham.Types.GameJson where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Act
import Arkham.Types.ActId
import Arkham.Types.Agenda
import Arkham.Types.AgendaId
import Arkham.Types.Asset
import Arkham.Types.AssetId
import Arkham.Types.Campaign
import Arkham.Types.Card
import Arkham.Types.Enemy
import Arkham.Types.EnemyId
import Arkham.Types.Event
import Arkham.Types.EventId
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Location
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Scenario
import Arkham.Types.Skill
import Arkham.Types.SkillId
import Arkham.Types.SkillTest
import Arkham.Types.Token
import Arkham.Types.Treachery
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.UUID

data GameJson = GameJson
  { gMessages :: [Message]
  , gSeed :: Int
  , gCampaign :: Maybe Campaign
  , gScenario :: Maybe Scenario
  , gLocations :: HashMap LocationId Location
  , gInvestigators :: HashMap InvestigatorId Investigator
  , gPlayers :: HashMap Int InvestigatorId
  , gEnemies :: HashMap EnemyId Enemy
  , gAssets :: HashMap AssetId Asset
  , gActiveInvestigatorId :: InvestigatorId
  , gLeadInvestigatorId :: InvestigatorId
  , gPhase :: Phase
  , gEncounterDeck :: Deck EncounterCard
  , gDiscard :: [EncounterCard]
  , gSkillTest :: Maybe (SkillTest Message)
  , gChaosBag :: Bag Token
  , gAgendas :: HashMap AgendaId Agenda
  , gTreacheries :: HashMap TreacheryId Treachery
  , gEvents :: HashMap EventId Event
  , gSkills :: HashMap SkillId Skill
  , gActs :: HashMap ActId Act
  , gGameOver :: Bool
  , gPending :: Bool
  , gPlayerCount :: Int
  , gUsedAbilities :: [(InvestigatorId, Ability)]
  , gQuestion :: HashMap InvestigatorId Question
  , gFocusedCards :: [Card]
  , gFocusedTokens :: [Token]
  , gActiveCard :: Maybe Card
  , gPlayerOrder :: [InvestigatorId]
  , gVictoryDisplay :: [Card]
  , gHash :: UUID
  }
  deriving stock (Show, Generic)

instance ToJSON GameJson where
  toJSON = genericToJSON $ aesonOptions $ Just "g"
  toEncoding = genericToEncoding $ aesonOptions $ Just "g"

instance FromJSON GameJson where
  parseJSON = genericParseJSON $ aesonOptions $ Just "g"


