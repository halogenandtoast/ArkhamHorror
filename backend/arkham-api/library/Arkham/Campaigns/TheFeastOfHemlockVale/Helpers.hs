{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Campaigns.TheFeastOfHemlockVale.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Criteria
import Arkham.Helpers.Campaign
import Arkham.Helpers.Log
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher.Scenario
import Arkham.Message (Message (NextCampaignStep))
import Arkham.Message.Lifted hiding (continue)
import Arkham.Message.Lifted.Log (decrementRecordCount, incrementRecordCount)
import Arkham.Modifier
import Arkham.Prelude hiding (Day)
import Arkham.Scenario.Options
import Arkham.Source
import Arkham.Target
import Arkham.Tracing

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theFeastOfHemlockVale" a

codex :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
codex iid (toSource -> source) n = scenarioSpecific "codex" (iid, source, n)

data Day = Day1 | Day2 | Day3
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Time = Night | Day
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

initMeta :: TheFeastOfHemlockValeMeta
initMeta = TheFeastOfHemlockValeMeta Day1 Day []

data TheFeastOfHemlockValeMeta = TheFeastOfHemlockValeMeta
  { day :: Day
  , time :: Time
  , chosenCodexEntries :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON TheFeastOfHemlockValeMeta where
  parseJSON = withObject "TheFeastOfHemlockValeMeta" $ \o -> do
    day <- o .: "day"
    time <- o .: "time"
    chosenCodexEntries <- o .:? "chosenCodexEntries" .!= []
    pure TheFeastOfHemlockValeMeta {..}

getCampaignTime :: (Tracing m, HasGame m) => m Time
getCampaignTime = withCampaignMeta @TheFeastOfHemlockValeMeta (.time)

getCampaignDay :: (Tracing m, HasGame m) => m Day
getCampaignDay = withCampaignMeta @TheFeastOfHemlockValeMeta (.day)

pattern IsDay :: Criterion
pattern IsDay <- ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Day")))
  where
    IsDay = ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Day")))

pattern IsNight :: Criterion
pattern IsNight <- ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Night")))
  where
    IsNight = ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Night")))

setScenarioDayAndTime :: ReverseQueue m => m ()
setScenarioDayAndTime = do
  day <- getCampaignDay
  time <- getCampaignTime
  gameModifier ScenarioSource ScenarioTarget (ScenarioModifierValue "day" (toJSON day))
  gameModifier ScenarioSource ScenarioTarget (ScenarioModifierValue "time" (toJSON time))

afterPrelude :: ReverseQueue m => CampaignStep -> m ()
afterPrelude =
  setNextCampaignStep . \case
    ScenarioStep sid ->
      ScenarioStepWithOptions sid defaultScenarioOptions {scenarioOptionsSkipInvestigatorSetup = True}
    other -> other
 where
  setNextCampaignStep = push . NextCampaignStep . continueNoUpgrade

getCrossedOutResidents :: (Tracing m, HasGame m) => m [Resident]
getCrossedOutResidents =
  catMaybes
    <$> sequence
      [ check SimeonCrossedOut SimeonAtwood
      , check LeahCrossedOut LeahAtwood
      , check TheoCrossedOut TheoPeters
      , check GideonCrossedOut GideonMizrah
      , check JudithCrossedOut JudithPark
      , check WilliamCrossedOut WilliamHemlock
      , check RiverCrossedOut RiverHawthorne
      , check MotherRachelCrossedOut MotherRachel
      ]
 where
  check k v = runMaybeT (liftGuardM (getHasRecord k) $> v)

pattern Theta :: Int
pattern Theta = 100

pattern Omega :: Int
pattern Omega = 101

pattern Psi :: Int
pattern Psi = 102

pattern Phi :: Int
pattern Phi = 103

pattern Sigma :: Int
pattern Sigma = 104

data Resident
  = WilliamHemlock
  | RiverHawthorne
  | MotherRachel
  | SimeonAtwood
  | LeahAtwood
  | TheoPeters
  | GideonMizrah
  | JudithPark
  deriving stock (Show, Eq, Bounded, Enum)

instance HasCardCode Resident where
  toCardCode = toCardCode . toCardDef

instance FetchCard Resident where
  fetchCardMaybe = fetchCardMaybe . toCardDef
  fetchCardMaybe_ = fetchCardMaybe_ . toCardDef

instance HasCardDef Resident where
  toCardDef = \case
    WilliamHemlock -> Assets.williamHemlockAspiringPoet
    RiverHawthorne -> Assets.riverHawthorneBigInNewYork
    MotherRachel -> Assets.motherRachelKindlyMatron
    SimeonAtwood -> Assets.simeonAtwoodDedicatedTroublemaker
    LeahAtwood -> Assets.leahAtwoodTheValeCook
    TheoPeters -> Assets.theoPetersJackOfAllTrades
    GideonMizrah -> Assets.gideonMizrahSeasonedSailor
    JudithPark -> Assets.judithParkTheMuscle

relationshipKey :: Resident -> CampaignLogKey
relationshipKey = \case
  WilliamHemlock -> toCampaignLogKey WilliamHemlockRelationshipLevel
  RiverHawthorne -> toCampaignLogKey RiverHawthorneRelationshipLevel
  MotherRachel -> toCampaignLogKey MotherRachelRelationshipLevel
  SimeonAtwood -> toCampaignLogKey SimeonAtwoodRelationshipLevel
  LeahAtwood -> toCampaignLogKey LeahAtwoodRelationshipLevel
  TheoPeters -> toCampaignLogKey TheoPetersRelationshipLevel
  GideonMizrah -> toCampaignLogKey GideonMizrahRelationshipLevel
  JudithPark -> toCampaignLogKey JudithParkRelationshipLevel

getRelationshipLevel :: (HasGame m, Tracing m) => Resident -> m Int
getRelationshipLevel = getRecordCount . relationshipKey

increaseRelationshipLevel :: ReverseQueue m => Resident -> Int -> m ()
increaseRelationshipLevel r = incrementRecordCount (relationshipKey r)

decreaseRelationshipLevel :: ReverseQueue m => Resident -> Int -> m ()
decreaseRelationshipLevel r = decrementRecordCount (relationshipKey r)
