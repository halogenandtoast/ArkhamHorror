{-# LANGUAGE TemplateHaskell #-}

module Arkham.Calculation where

import {-# SOURCE #-} Arkham.Asset.Types (Asset)
import Arkham.CampaignLogKey
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Enemy.Types (Enemy)
import Arkham.Field
import Arkham.GameValue
import Arkham.Id
import {-# SOURCE #-} Arkham.Investigator.Types (Investigator)
import {-# SOURCE #-} Arkham.Location.Types (Location)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.SlotType
import Arkham.Token
import Data.Aeson.TH

data GameCalculation
  = Fixed Int
  | MaxCalculation GameCalculation GameCalculation
  | DividedByCalculation GameCalculation Int
  | SumCalculation [GameCalculation]
  | SubtractCalculation GameCalculation GameCalculation
  | RecordedCount CampaignLogKey
  | ScenarioCount ScenarioCountKey
  | CountActs ActMatcher
  | CountAgendas AgendaMatcher
  | CountAssets AssetMatcher
  | CountEnemies EnemyMatcher
  | CountEvents EventMatcher
  | CountInvestigators InvestigatorMatcher
  | CountLocations LocationMatcher
  | CountSkills SkillMatcher
  | CountTreacheries TreacheryMatcher
  | CurrentAgendaStepCalculation GameCalculation
  | AssetFieldCalculation AssetId (Field Asset Int)
  | InvestigatorFieldCalculation InvestigatorId (Field Investigator Int)
  | InvestigatorHandLengthCalculation InvestigatorId
  | EnemyMaybeFieldCalculation EnemyId (Field Enemy (Maybe Int))
  | EnemyMaybeGameValueFieldCalculation EnemyId (Field Enemy (Maybe GameValue))
  | EnemyFieldCalculation EnemyId (Field Enemy Int)
  | VictoryDisplayCountCalculation CardMatcher
  | LocationFieldCalculation LocationId (Field Location Int)
  | LocationMaybeFieldCalculation LocationId (Field Location (Maybe Int))
  | InvestigatorLocationFieldCalculation InvestigatorId (Field Location Int)
  | InvestigatorLocationMaybeFieldCalculation InvestigatorId (Field Location (Maybe Int))
  | CardCostCalculation InvestigatorId CardId
  | ScenarioInDiscardCountCalculation CardMatcher
  | DoomCountCalculation
  | DistanceFromCalculation InvestigatorId LocationMatcher
  | InvestigatorTokenCountCalculation InvestigatorId Token
  | AssetTokenCountCalculation AssetId Token
  | MaxAlarmLevelCalculation -- getMaxAlarmLevel
  | VengeanceCalculation -- getVengeanceInVictoryDisplay
  | DifferentClassAmong ExtendedCardMatcher
  | EnemyTargetFieldCalculation (Field Enemy Int)
  | CountChaosTokens ChaosTokenMatcher
  | GameValueCalculation GameValue
  | DuringEventCalculation GameCalculation GameCalculation
  | EmptySlotsCalculation InvestigatorMatcher SlotType
  deriving stock (Show, Ord, Eq, Data, Generic)
  deriving FromJSON via MaybeFixed

newtype MaybeFixed = MaybeFixed GameCalculation

instance FromJSON MaybeFixed where
  parseJSON = \case
    v@(Number _) -> MaybeFixed . Fixed <$> parseJSON v
    other -> MaybeFixed <$> genericParseJSON defaultOptions other

$(deriveToJSON defaultOptions ''GameCalculation)
