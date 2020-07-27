module Arkham.Types.GameJson where

import Arkham.Types.Ability
import Arkham.Types.Act
import Arkham.Types.ActId
import Arkham.Types.Agenda
import Arkham.Types.AgendaId
import Arkham.Types.Asset
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Enemy
import Arkham.Types.EnemyId
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Location
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Scenario
import Arkham.Types.SkillTest
import Arkham.Types.Token
import Arkham.Types.Treachery
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data GameJson = GameJson
  { gMessages :: [Message]
  , gSeed :: Int
  , gScenario :: Scenario
  , gLocations :: HashMap LocationId Location
  , gInvestigators :: HashMap InvestigatorId Investigator
  , gEnemies :: HashMap EnemyId Enemy
  , gAssets :: HashMap AssetId Asset
  , gActiveInvestigatorId :: InvestigatorId
  , gLeadInvestigatorId :: InvestigatorId
  , gPhase :: Phase
  , gEncounterDeck :: Deck EncounterCard
  , gDiscard :: [EncounterCard]
  , gSkillTest :: Maybe SkillTest
  , gChaosBag :: Bag Token
  , gAgendas :: HashMap AgendaId Agenda
  , gTreacheries :: HashMap TreacheryId Treachery
  , gActs :: HashMap ActId Act
  , gGameOver :: Bool
  , gUsedAbilities :: [Ability]
  , gQuestion :: Maybe Question
  }
  deriving stock (Show, Generic)

instance ToJSON GameJson where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 1 }
  toEncoding = genericToEncoding $ defaultOptions { fieldLabelModifier = camelCase . drop 1 }

instance FromJSON GameJson where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 1 }


