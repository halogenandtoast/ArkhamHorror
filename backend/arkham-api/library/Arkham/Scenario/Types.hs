{-# LANGUAGE TemplateHaskell #-}

module Arkham.Scenario.Types (module Arkham.Scenario.Types, module X, Field (..)) where

import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Classes.Entity
import Arkham.Classes.HasChaosTokenValue
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.History
import Arkham.I18n
import Arkham.Id
import Arkham.Json
import Arkham.Key
import Arkham.Layout
import Arkham.Location.Grid
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck as X
import Arkham.ScenarioLogKey
import Arkham.Search
import Arkham.Search qualified as Search
import Arkham.Source
import Arkham.Target
import Arkham.Tarot
import Arkham.Token
import Arkham.Xp
import Arkham.Zone
import Control.Lens (_Just)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH
import Data.Data
import Data.Text qualified as T
import GHC.Records

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasModifiersFor a
  , RunMessage a
  , HasChaosTokenValue a
  , Entity a
  , EntityId a ~ ScenarioId
  , EntityAttrs a ~ ScenarioAttrs
  , RunType a ~ a
  ) =>
  IsScenario a

data instance Field Scenario :: Type -> Type where
  ScenarioCardsUnderActDeck :: Field Scenario [Card]
  ScenarioCardsNextToActDeck :: Field Scenario [Card]
  ScenarioCardsUnderAgendaDeck :: Field Scenario [Card]
  ScenarioCardsUnderScenarioReference :: Field Scenario [Card]
  ScenarioDiscard :: Field Scenario [EncounterCard]
  ScenarioEncounterDeck :: Field Scenario (Deck EncounterCard)
  ScenarioHasEncounterDeck :: Field Scenario Bool
  ScenarioDifficulty :: Field Scenario Difficulty
  ScenarioDecks :: Field Scenario (Map ScenarioDeckKey [Card])
  ScenarioVictoryDisplay :: Field Scenario [Card]
  ScenarioRemembered :: Field Scenario (Set ScenarioLogKey)
  ScenarioCounts :: Field Scenario (Map ScenarioCountKey Int)
  ScenarioEncounterDecks
    :: Field Scenario (Map ScenarioEncounterDeckKey (Deck EncounterCard, [EncounterCard]))
  ScenarioStandaloneCampaignLog :: Field Scenario CampaignLog
  ScenarioResignedCardCodes :: Field Scenario [CardCode]
  ScenarioResolvedStories :: Field Scenario [StoryId]
  ScenarioChaosBag :: Field Scenario ChaosBag
  ScenarioInResolution :: Field Scenario Bool
  ScenarioSetAsideCards :: Field Scenario [Card]
  ScenarioSetAsideKeys :: Field Scenario (Set ArkhamKey)
  ScenarioKeys :: Field Scenario (Set ArkhamKey)
  ScenarioName :: Field Scenario Name
  ScenarioMeta :: Field Scenario Value
  ScenarioStoryCards :: Field Scenario (Map InvestigatorId [Card])
  ScenarioPlayerDecks :: Field Scenario (Map InvestigatorId (Deck PlayerCard))
  ScenarioTokens :: Field Scenario Tokens
  ScenarioTarotCards :: Field Scenario (Map TarotCardScope [TarotCard])
  ScenarioTurn :: Field Scenario Int
  ScenarioDefeatedEnemies :: Field Scenario (Map EnemyId DefeatedEnemyAttrs)
  ScenarioGrid :: Field Scenario Grid
  ScenarioLocationLayout :: Field Scenario [GridTemplateRow]

deriving stock instance Show (Field Scenario typ)

data ScenarioAttrs = ScenarioAttrs
  { scenarioName :: Name
  , scenarioId :: ScenarioId
  , scenarioReference :: CardCode
  , scenarioAdditionalReferences :: [CardCode]
  , scenarioDifficulty :: Difficulty
  , scenarioCardsUnderScenarioReference :: [Card]
  , scenarioCardsUnderAgendaDeck :: [Card]
  , scenarioCardsUnderActDeck :: [Card]
  , scenarioCardsNextToActDeck :: [Card]
  , scenarioCardsNextToAgendaDeck :: [Card]
  , scenarioActStack :: IntMap [Card]
  , scenarioAgendaStack :: IntMap [Card]
  , scenarioCompletedAgendaStack :: IntMap [Card]
  , scenarioCompletedActStack :: IntMap [Card]
  , scenarioLocationLayout :: [GridTemplateRow]
  , scenarioGrid :: Grid
  , scenarioDecks :: Map ScenarioDeckKey [Card]
  , scenarioLog :: Set ScenarioLogKey
  , scenarioCounts :: Map ScenarioCountKey Int
  , scenarioStandaloneCampaignLog :: CampaignLog
  , scenarioSetAsideCards :: [Card]
  , scenarioInResolution :: Bool
  , scenarioNoRemainingInvestigatorsHandler :: Target
  , scenarioVictoryDisplay :: [Card]
  , scenarioChaosBag :: ChaosBag
  , scenarioEncounterDeck :: Deck EncounterCard
  , scenarioHasEncounterDeck :: Bool
  , scenarioDiscard :: [EncounterCard]
  , scenarioEncounterDecks :: Map ScenarioEncounterDeckKey (Deck EncounterCard, [EncounterCard])
  , scenarioActiveEncounterDeck :: ScenarioEncounterDeckKey
  , scenarioResignedCardCodes :: [CardCode]
  , scenarioResolvedStories :: [StoryId]
  , scenarioDecksLayout :: [GridTemplateRow]
  , scenarioSetAsideKeys :: Set ArkhamKey
  , scenarioKeys :: Set ArkhamKey
  , scenarioMeta :: Value
  , scenarioUsesGrid :: Bool
  , scenarioTokens :: Tokens
  , scenarioTarotCards :: Map TarotCardScope [TarotCard]
  , scenarioTarotDeck :: [TarotCardArcana]
  , scenarioTurn :: Int
  , scenarioTimesPlayed :: Int
  , scenarioDefeatedEnemies :: Map EnemyId DefeatedEnemyAttrs
  , scenarioIsSideStory :: Bool
  , scenarioInShuffle :: Bool
  , scenarioSearch :: Maybe Search
  , scenarioStarted :: Bool
  , scenarioScope :: Scope
  , -- for standalone
    scenarioStoryCards :: Map InvestigatorId [Card]
  , scenarioPlayerDecks :: Map InvestigatorId (Deck PlayerCard)
  , scenarioXpBreakdown :: Maybe XpBreakdown
  }
  deriving stock (Show, Eq)

instance AsId ScenarioAttrs where
  type IdOf ScenarioAttrs = ScenarioId
  asId = scenarioId

instance HasField "id" ScenarioAttrs ScenarioId where
  getField = scenarioId

instance HasField "standaloneCampaignLog" ScenarioAttrs CampaignLog where
  getField = scenarioStandaloneCampaignLog

instance HasField "hasOption" ScenarioAttrs (CampaignOption -> Bool) where
  getField s k = k `member` s.standaloneCampaignLog.options

instance HasField "tokens" ScenarioAttrs Tokens where
  getField = scenarioTokens

instance HasField "token" ScenarioAttrs (Token -> Int) where
  getField a tkn = countTokens tkn a.tokens

instance HasField "discard" ScenarioAttrs [EncounterCard] where
  getField = scenarioDiscard

instance HasField "cardsUnderActDeck" ScenarioAttrs [Card] where
  getField = scenarioCardsUnderActDeck

instance HasField "cardsUnderAgendaDeck" ScenarioAttrs [Card] where
  getField = scenarioCardsUnderAgendaDeck

instance HasField "completedActStack" ScenarioAttrs (IntMap [Card]) where
  getField = scenarioCompletedActStack

instance HasField "completedAgendaStack" ScenarioAttrs (IntMap [Card]) where
  getField = scenarioCompletedAgendaStack

instance HasField "difficulty" ScenarioAttrs Difficulty where
  getField = scenarioDifficulty

instance HasField "setAside" ScenarioAttrs [Card] where
  getField = scenarioSetAsideCards

instance HasField "resignedCardCodes" ScenarioAttrs [CardCode] where
  getField = scenarioResignedCardCodes

instance HasField "decks" ScenarioAttrs (Map ScenarioDeckKey [Card]) where
  getField = scenarioDecks

instance HasField "deck" ScenarioAttrs (ScenarioDeckKey -> [Card]) where
  getField a k = findWithDefault [] k a.decks

instance HasField "log" ScenarioAttrs (Set ScenarioLogKey) where
  getField = scenarioLog

instance HasField "name" ScenarioAttrs Name where
  getField = scenarioName

instance HasField "grid" ScenarioAttrs Grid where
  getField = scenarioGrid

instance HasField "meta" ScenarioAttrs Value where
  getField = scenarioMeta

getMetaKeyDefault :: FromJSON a => Key -> a -> ScenarioAttrs -> a
getMetaKeyDefault k def attrs = case attrs.meta of
  Object o -> case KeyMap.lookup k o of
    Nothing -> def
    Just v -> case fromJSON v of
      Error _ -> def
      Success v' -> v'
  _ -> def

setMetaKey :: (ToJSON a, HasCallStack) => Key -> a -> ScenarioAttrs -> ScenarioAttrs
setMetaKey k v attrs = case attrs.meta of
  Object o -> attrs {scenarioMeta = Object $ KeyMap.insert k (toJSON v) o}
  Null -> attrs {scenarioMeta = object [k .= v]}
  _ -> error $ "Could not insert meta key, meta is not Null or Object: " <> show attrs.meta

instance HasField "id" Scenario ScenarioId where
  getField = (.id) . toAttrs

instance HasField "difficulty" Scenario Difficulty where
  getField = (.difficulty) . toAttrs

instance HasField "name" Scenario Name where
  getField = (.name) . toAttrs

setStandaloneCampaignLog :: CampaignLog -> ScenarioAttrs -> ScenarioAttrs
setStandaloneCampaignLog standaloneCampaignLog attrs =
  if emptyCampaignLog (scenarioStandaloneCampaignLog attrs)
    then attrs {scenarioStandaloneCampaignLog = standaloneCampaignLog}
    else attrs

scenarioWith
  :: (ScenarioAttrs -> a)
  -> CardCode
  -> Name
  -> Difficulty
  -> [GridTemplateRow]
  -> (ScenarioAttrs -> ScenarioAttrs)
  -> a
scenarioWith f cardCode name difficulty layout g =
  scenario (f . g) cardCode name difficulty layout

sideStory
  :: (ScenarioAttrs -> a)
  -> CardCode
  -> Name
  -> Difficulty
  -> [GridTemplateRow]
  -> a
sideStory f cardCode name difficulty layout =
  scenario (f . setSideStory) cardCode name difficulty layout
 where
  setSideStory attrs = attrs {scenarioIsSideStory = True}

scenario
  :: (ScenarioAttrs -> a)
  -> CardCode
  -> Name
  -> Difficulty
  -> [GridTemplateRow]
  -> a
scenario f cardCode name difficulty layout =
  f
    $ ScenarioAttrs
      { scenarioId = ScenarioId cardCode
      , scenarioReference = cardCode
      , scenarioAdditionalReferences = []
      , scenarioName = name
      , scenarioDifficulty = difficulty
      , scenarioCompletedAgendaStack = mempty
      , scenarioCompletedActStack = mempty
      , scenarioAgendaStack = mempty
      , scenarioActStack = mempty
      , scenarioCardsUnderAgendaDeck = mempty
      , scenarioCardsUnderActDeck = mempty
      , scenarioCardsNextToActDeck = mempty
      , scenarioCardsNextToAgendaDeck = mempty
      , scenarioLocationLayout = layout
      , scenarioGrid = initGrid
      , scenarioDecks = mempty
      , scenarioLog = mempty
      , scenarioCounts = mempty
      , scenarioSetAsideCards = mempty
      , scenarioStandaloneCampaignLog = mkCampaignLog
      , scenarioCardsUnderScenarioReference = mempty
      , scenarioInResolution = False
      , scenarioNoRemainingInvestigatorsHandler = ScenarioTarget
      , scenarioVictoryDisplay = mempty
      , scenarioChaosBag = emptyChaosBag
      , scenarioEncounterDeck = mempty
      , scenarioEncounterDecks = mempty
      , scenarioHasEncounterDeck = True
      , scenarioActiveEncounterDeck = RegularEncounterDeck
      , scenarioDiscard = mempty
      , scenarioResignedCardCodes = mempty
      , scenarioResolvedStories = mempty
      , scenarioDecksLayout = ["agenda1 act1"]
      , scenarioSetAsideKeys = mempty
      , scenarioKeys = mempty
      , scenarioMeta = Null
      , scenarioUsesGrid = False
      , scenarioTokens = mempty
      , scenarioStoryCards = mempty
      , scenarioPlayerDecks = mempty
      , scenarioTarotCards = mempty
      , scenarioTarotDeck = mempty
      , scenarioTurn = 0
      , scenarioTimesPlayed = 0
      , scenarioDefeatedEnemies = mempty
      , scenarioIsSideStory = False
      , scenarioInShuffle = False
      , scenarioXpBreakdown = Nothing
      , scenarioSearch = Nothing
      , scenarioStarted = False
      , scenarioScope = toScope $ case T.stripPrefix "Return to " (toTitle name) of
          Just suffix -> suffix
          Nothing -> toTitle name
      }

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = ScenarioId
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs = id
  overAttrs f = f

instance Named ScenarioAttrs where
  toName = scenarioName

instance Targetable ScenarioAttrs where
  toTarget _ = ScenarioTarget
  isTarget _ ScenarioTarget = True
  isTarget _ _ = False

instance Sourceable ScenarioAttrs where
  toSource _ = ScenarioSource
  isSource _ ScenarioSource = True
  isSource _ _ = False

data Scenario where
  Scenario :: IsScenario a => a -> Scenario

instance Data Scenario where
  gunfold _ _ _ = error "gunfold(Scenario)"
  toConstr _ = error "toConstr(Scenario)"
  dataTypeOf _ = error "dataTypeOf(Scenario)"

instance Targetable Scenario where
  toTarget _ = ScenarioTarget

instance Eq Scenario where
  Scenario (a :: a) == Scenario (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Scenario where
  show (Scenario a) = show a

instance ToJSON Scenario where
  toJSON (Scenario a) = toJSON a

instance Entity Scenario where
  type EntityId Scenario = ScenarioId
  type EntityAttrs Scenario = ScenarioAttrs
  toId = toId . toAttrs
  toAttrs (Scenario a) = toAttrs a
  overAttrs f (Scenario a) = Scenario $ overAttrs f a

difficultyOfScenario :: Scenario -> Difficulty
difficultyOfScenario = scenarioDifficulty . toAttrs

scenarioActs :: Scenario -> [Card]
scenarioActs s = case mapToList $ scenarioActStack (toAttrs s) of
  [(_, actIds)] -> actIds
  _ -> error "Not able to handle multiple act stacks yet"

makeLensesWith suffixedFields ''ScenarioAttrs

foundCardsL :: Traversal' ScenarioAttrs (Map Zone [Card])
foundCardsL = searchL . _Just . Search.foundCardsL

$(deriveToJSON (aesonOptions $ Just "scenario") ''ScenarioAttrs)

instance FromJSON ScenarioAttrs where
  parseJSON = withObject "ScenarioAttrs" \o -> do
    scenarioName <- o .: "name"
    scenarioId <- o .: "id"
    scenarioReference <- o .: "reference"
    scenarioAdditionalReferences <- o .:? "additionalReferences" .!= mempty
    scenarioDifficulty <- o .: "difficulty"
    scenarioCardsUnderScenarioReference <- o .: "cardsUnderScenarioReference"
    scenarioCardsUnderAgendaDeck <- o .: "cardsUnderAgendaDeck"
    scenarioCardsUnderActDeck <- o .: "cardsUnderActDeck"
    scenarioCardsNextToActDeck <- o .: "cardsNextToActDeck"
    scenarioCardsNextToAgendaDeck <- o .: "cardsNextToAgendaDeck"
    scenarioActStack <- o .: "actStack"
    scenarioAgendaStack <- o .: "agendaStack"
    scenarioCompletedAgendaStack <- o .: "completedAgendaStack"
    scenarioCompletedActStack <- o .: "completedActStack"
    scenarioLocationLayout <- o .: "locationLayout"
    scenarioGrid <- o .:? "grid" .!= initGrid
    scenarioDecks <- o .: "decks"
    scenarioLog <- o .: "log"
    scenarioCounts <- o .: "counts"
    scenarioStandaloneCampaignLog <- o .: "standaloneCampaignLog"
    scenarioSetAsideCards <- o .: "setAsideCards"
    scenarioInResolution <- o .: "inResolution"
    scenarioNoRemainingInvestigatorsHandler <- o .: "noRemainingInvestigatorsHandler"
    scenarioVictoryDisplay <- o .: "victoryDisplay"
    scenarioChaosBag <- o .: "chaosBag"
    scenarioEncounterDeck <- o .: "encounterDeck"
    scenarioHasEncounterDeck <- o .: "hasEncounterDeck"
    scenarioDiscard <- o .: "discard"
    scenarioEncounterDecks <- o .: "encounterDecks"
    scenarioActiveEncounterDeck <- o .: "activeEncounterDeck"
    scenarioResignedCardCodes <- o .: "resignedCardCodes"
    scenarioResolvedStories <- o .: "resolvedStories"
    scenarioDecksLayout <- o .: "decksLayout"
    scenarioSetAsideKeys <- o .: "setAsideKeys"
    scenarioKeys <- o .:? "keys" .!= mempty
    scenarioMeta <- o .: "meta"
    scenarioUsesGrid <- o .: "usesGrid"
    scenarioTokens <- o .: "tokens"
    scenarioTarotCards <- o .: "tarotCards"
    scenarioTarotDeck <- o .: "tarotDeck"
    scenarioTurn <- o .: "turn"
    scenarioTimesPlayed <- o .: "timesPlayed"
    scenarioDefeatedEnemies <- o .: "defeatedEnemies"
    scenarioIsSideStory <- o .:? "isSideStory" .!= False
    scenarioInShuffle <- o .:? "inShuffle" .!= False
    scenarioStoryCards :: Map InvestigatorId [Card] <-
      (o .: "storyCards") <|> (map (toCard @PlayerCard) <$$> (o .: "storyCards"))
    scenarioPlayerDecks <- o .: "playerDecks"
    scenarioXpBreakdown <- o .:? "xpBreakdown"
    scenarioSearch <- o .:? "search"
    scenarioStarted <- o .:? "started" .!= True
    scenarioScope <- o .:? "scope" .!= "missing"
    pure ScenarioAttrs {..}
