{-# LANGUAGE TemplateHaskell #-}

module Arkham.Calculation where

import {-# SOURCE #-} Arkham.Act.Types (Act)
import {-# SOURCE #-} Arkham.Asset.Types (Asset)
import Arkham.CampaignLogKey
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Enemy.Types (Enemy)
import Arkham.Field
import Arkham.GameValue
import Arkham.Id
import {-# SOURCE #-} Arkham.Investigator.Types (Investigator)
import {-# SOURCE #-} Arkham.Location.Types (Location)
import Arkham.Matcher.Act
import Arkham.Matcher.Agenda
import Arkham.Matcher.Asset
import Arkham.Matcher.Card
import Arkham.Matcher.ChaosToken
import Arkham.Matcher.Enemy
import Arkham.Matcher.Event
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Skill
import Arkham.Matcher.Treachery
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.SlotType
import Arkham.Token
import Data.Aeson.TH

data GameCalculation
  = Negated GameCalculation
  | Fixed Int
  | MaxCalculation GameCalculation GameCalculation
  | DividedByCalculation GameCalculation Int
  | SumCalculation [GameCalculation]
  | SubtractCalculation GameCalculation GameCalculation
  | MultiplyCalculation GameCalculation GameCalculation
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
  | ActFieldCalculation ActId (Field Act Int)
  | InvestigatorFieldCalculation InvestigatorId (Field Investigator Int)
  | InvestigatorsFieldCalculation InvestigatorMatcher (Field Investigator Int)
  | InvestigatorKeyCountCalculation InvestigatorMatcher
  | LocationKeyCountCalculation LocationMatcher
  | InvestigatorHandLengthCalculation InvestigatorId
  | EnemyMaybeFieldCalculation EnemyId (Field Enemy (Maybe Int))
  | SumEnemyMaybeFieldCalculation EnemyMatcher (Field Enemy (Maybe Int))
  | EnemyMaybeGameValueFieldCalculation EnemyId (Field Enemy (Maybe GameCalculation))
  | EnemyFieldCalculation EnemyId (Field Enemy Int)
  | VictoryDisplayCountCalculation ExtendedCardMatcher
  | LocationFieldCalculation LocationId (Field Location Int)
  | LocationGameValueFieldCalculation LocationId (Field Location GameValue)
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
  | DifferentClassAmong InvestigatorMatcher ExtendedCardMatcher
  | EnemyTargetFieldCalculation (Field Enemy Int)
  | CountChaosTokens ChaosTokenMatcher
  | GameValueCalculation GameValue
  | DuringEventCalculation GameCalculation GameCalculation
  | EmptySlotsCalculation InvestigatorMatcher SlotType
  | AmountYouOweToBiancaDieKatz InvestigatorMatcher
  | IfLocationExistsCalculation LocationMatcher GameCalculation GameCalculation
  | IfEnemyExistsCalculation EnemyMatcher GameCalculation GameCalculation
  deriving stock (Show, Ord, Eq, Data, Generic)
  deriving FromJSON via MaybeFixed

recordedCount :: IsCampaignLogKey k => k -> GameCalculation
recordedCount = RecordedCount . toCampaignLogKey

newtype MaybeFixed = MaybeFixed GameCalculation

instance FromJSON MaybeFixed where
  parseJSON = \case
    v@(Number _) -> MaybeFixed . Fixed <$> parseJSON v
    other -> MaybeFixed <$> genericParseJSON defaultOptions other

$(deriveToJSON defaultOptions ''GameCalculation)
