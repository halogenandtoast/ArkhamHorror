{-# LANGUAGE TemplateHaskell #-}

module Arkham.Scenario.Types (
  module Arkham.Scenario.Types,
  module X,
  Field (..),
) where

import Arkham.Prelude

import Arkham.CampaignLog
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Classes.Entity
import Arkham.Classes.HasChaosTokenValue
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.Id
import Arkham.Json
import Arkham.Key
import Arkham.Layout
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Deck as X
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Target
import Arkham.Tarot
import Arkham.Token
import Data.Aeson.TH
import Data.Typeable

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , NoThunks a
  , HasModifiersFor a
  , RunMessage a
  , HasChaosTokenValue a
  , Entity a
  , EntityId a ~ ScenarioId
  , EntityAttrs a ~ ScenarioAttrs
  ) =>
  IsScenario a

data instance Field Scenario :: Type -> Type where
  ScenarioCardsUnderActDeck :: Field Scenario [Card]
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
  ScenarioSetAsideCards :: Field Scenario [Card]
  ScenarioSetAsideKeys :: Field Scenario (Set ArkhamKey)
  ScenarioName :: Field Scenario Name
  ScenarioMeta :: Field Scenario Value
  ScenarioStoryCards :: Field Scenario (Map InvestigatorId [PlayerCard])
  ScenarioPlayerDecks :: Field Scenario (Map InvestigatorId (Deck PlayerCard))
  ScenarioTokens :: Field Scenario Tokens
  ScenarioTarotCards :: Field Scenario (Map TarotCardScope [TarotCard])
  ScenarioTurn :: Field Scenario Int

deriving stock instance Show (Field Scenario typ)

data ScenarioAttrs = ScenarioAttrs
  { scenarioName :: Name
  , scenarioId :: ScenarioId
  , scenarioReference :: CardCode
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
  , scenarioMeta :: Value
  , scenarioUsesGrid :: Bool
  , scenarioTokens :: Tokens
  , scenarioTarotCards :: Map TarotCardScope [TarotCard]
  , scenarioTarotDeck :: [TarotCardArcana]
  , scenarioTurn :: Int
  , scenarioTimesPlayed :: Int
  , -- for standalone
    scenarioStoryCards :: Map InvestigatorId [PlayerCard]
  , scenarioPlayerDecks :: Map InvestigatorId (Deck PlayerCard)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks)

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
      , scenarioMeta = Null
      , scenarioUsesGrid = False
      , scenarioTokens = mempty
      , scenarioStoryCards = mempty
      , scenarioPlayerDecks = mempty
      , scenarioTarotCards = mempty
      , scenarioTarotDeck = mempty
      , scenarioTurn = 0
      , scenarioTimesPlayed = 0
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

instance NoThunks Scenario where
  noThunks ctx (Scenario a) = noThunks ctx a
  wNoThunks ctx (Scenario a) = wNoThunks ctx a
  showTypeOf _ = "Scenario"

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

instance FromJSON ScenarioAttrs where
  parseJSON = withObject "ScenarioAttrs" $ \o -> do
    scenarioId <- o .: "id"
    scenarioReference <- o .: "reference"
    scenarioName <- o .: "name"
    scenarioDifficulty <- o .: "difficulty"
    scenarioCompletedAgendaStack <- o .: "completedAgendaStack"
    scenarioCompletedActStack <- o .: "completedActStack"
    scenarioAgendaStack <- o .: "agendaStack"
    scenarioActStack <- o .: "actStack"
    scenarioCardsUnderAgendaDeck <- o .: "cardsUnderAgendaDeck"
    scenarioCardsUnderActDeck <- o .: "cardsUnderActDeck"
    scenarioCardsNextToActDeck <- o .: "cardsNextToActDeck"
    scenarioCardsNextToAgendaDeck <- o .: "cardsNextToAgendaDeck"
    scenarioLocationLayout <- o .: "locationLayout"
    scenarioDecks <- o .: "decks"
    scenarioLog <- o .: "log"
    scenarioCounts <- o .: "counts"
    scenarioSetAsideCards <- o .: "setAsideCards"
    scenarioStandaloneCampaignLog <- o .: "standaloneCampaignLog"
    scenarioCardsUnderScenarioReference <- o .: "cardsUnderScenarioReference"
    scenarioInResolution <- o .: "inResolution"
    scenarioNoRemainingInvestigatorsHandler <- o .: "noRemainingInvestigatorsHandler"
    scenarioVictoryDisplay <- o .: "victoryDisplay"
    scenarioChaosBag <- o .: "chaosBag"
    scenarioEncounterDeck <- o .: "encounterDeck"
    scenarioEncounterDecks <- o .: "encounterDecks"
    scenarioHasEncounterDeck <- o .:? "hasEncounterDeck" .!= True
    scenarioActiveEncounterDeck <- o .: "activeEncounterDeck"
    scenarioDiscard <- o .: "discard"
    scenarioResignedCardCodes <- o .: "resignedCardCodes"
    scenarioResolvedStories <- o .: "resolvedStories"
    scenarioDecksLayout <- o .: "decksLayout"
    scenarioSetAsideKeys <- o .: "setAsideKeys"
    scenarioMeta <- o .: "meta"
    scenarioUsesGrid <- o .: "usesGrid"
    scenarioTokens <- o .: "tokens"
    scenarioStoryCards <- o .: "storyCards"
    scenarioPlayerDecks <- o .: "playerDecks"
    scenarioTarotCards <- o .: "tarotCards"
    scenarioTarotDeck <- o .: "tarotDeck"
    scenarioTurn <- o .: "turn"
    scenarioTimesPlayed <- o .: "timesPlayed"
    pure ScenarioAttrs {..}

makeLensesWith suffixedFields ''ScenarioAttrs
$(deriveToJSON (aesonOptions $ Just "scenario") ''ScenarioAttrs)
