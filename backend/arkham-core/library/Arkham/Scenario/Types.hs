module Arkham.Scenario.Types
  ( module Arkham.Scenario.Types
  , module X
  , Field(..)
  ) where

import Arkham.Prelude

import Arkham.CampaignLog
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Classes.Entity
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasTokenValue
import Arkham.Classes.RunMessage.Internal
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Deck as X
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Target
import Data.Typeable

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasModifiersFor a, RunMessage a, HasTokenValue a, Entity a, EntityId a ~ ScenarioId, EntityAttrs a ~ ScenarioAttrs) => IsScenario a

newtype GridTemplateRow = GridTemplateRow { unGridTemplateRow :: Text }
  deriving newtype (Show, IsString, ToJSON, FromJSON, Eq)

data instance Field Scenario :: Type -> Type where
  ScenarioCardsUnderActDeck :: Field Scenario [Card]
  ScenarioCardsUnderAgendaDeck :: Field Scenario [Card]
  ScenarioCardsUnderScenarioReference :: Field Scenario [Card]
  ScenarioDiscard :: Field Scenario [EncounterCard]
  ScenarioEncounterDeck :: Field Scenario (Deck EncounterCard)
  ScenarioDifficulty :: Field Scenario Difficulty
  ScenarioDecks :: Field Scenario (HashMap ScenarioDeckKey [Card])
  ScenarioVictoryDisplay :: Field Scenario [Card]
  ScenarioRemembered :: Field Scenario (HashSet ScenarioLogKey)
  ScenarioCounts :: Field Scenario (HashMap ScenarioCountKey Int)
  ScenarioStandaloneCampaignLog :: Field Scenario CampaignLog
  ScenarioResignedCardCodes :: Field Scenario [CardCode]
  ScenarioChaosBag :: Field Scenario ChaosBag
  ScenarioSetAsideCards :: Field Scenario [Card]
  ScenarioName :: Field Scenario Name
  ScenarioMeta :: Field Scenario Value
  ScenarioStoryCards :: Field Scenario (HashMap InvestigatorId [PlayerCard])

deriving stock instance Show (Field Scenario typ)

data ScenarioAttrs = ScenarioAttrs
  { scenarioName :: Name
  , scenarioId :: ScenarioId
  , scenarioDifficulty :: Difficulty
  , scenarioCardsUnderScenarioReference :: [Card]
  , scenarioCardsUnderAgendaDeck :: [Card]
  , scenarioCardsUnderActDeck :: [Card]
  , scenarioCardsNextToActDeck :: [Card]
  , scenarioActStack :: IntMap [Card]
  , scenarioAgendaStack :: IntMap [Card]
  , scenarioCompletedAgendaStack :: IntMap [Card]
  , scenarioCompletedActStack :: IntMap [Card]
  , scenarioLocationLayout :: [GridTemplateRow]
  , scenarioDecks :: HashMap ScenarioDeckKey [Card]
  , scenarioLog :: HashSet ScenarioLogKey
  , scenarioCounts :: HashMap ScenarioCountKey Int
  , scenarioStandaloneCampaignLog :: CampaignLog
  , scenarioSetAsideCards :: [Card]
  , scenarioInResolution :: Bool
  , scenarioNoRemainingInvestigatorsHandler :: Target
  , scenarioVictoryDisplay :: [Card]
  , scenarioChaosBag :: ChaosBag
  , scenarioEncounterDeck :: Deck EncounterCard
  , scenarioDiscard :: [EncounterCard]
  , scenarioResignedCardCodes :: [CardCode]
  , scenarioDecksLayout :: [GridTemplateRow]
  , scenarioMeta :: Value
  -- for standalone
  , scenarioStoryCards :: HashMap InvestigatorId [PlayerCard]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ScenarioAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "scenario"

instance FromJSON ScenarioAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "scenario"

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
scenario f cardCode name difficulty layout = f $ ScenarioAttrs
  { scenarioId = ScenarioId cardCode
  , scenarioName = name
  , scenarioDifficulty = difficulty
  , scenarioCompletedAgendaStack = mempty
  , scenarioCompletedActStack = mempty
  , scenarioAgendaStack = mempty
  , scenarioActStack = mempty
  , scenarioCardsUnderAgendaDeck = mempty
  , scenarioCardsUnderActDeck = mempty
  , scenarioCardsNextToActDeck = mempty
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
  , scenarioDiscard = mempty
  , scenarioResignedCardCodes = mempty
  , scenarioDecksLayout = ["agenda1 act1"]
  , scenarioMeta = Null
  , scenarioStoryCards = mempty
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

nameL :: Lens' ScenarioAttrs Name
nameL = lens scenarioName $ \m x -> m { scenarioName = x }

idL :: Lens' ScenarioAttrs ScenarioId
idL = lens scenarioId $ \m x -> m { scenarioId = x }

metaL :: Lens' ScenarioAttrs Value
metaL = lens scenarioMeta $ \m x -> m { scenarioMeta = x }

difficultyL :: Lens' ScenarioAttrs Difficulty
difficultyL = lens scenarioDifficulty $ \m x -> m { scenarioDifficulty = x }

cardsUnderScenarioReferenceL :: Lens' ScenarioAttrs [Card]
cardsUnderScenarioReferenceL = lens scenarioCardsUnderScenarioReference
  $ \m x -> m { scenarioCardsUnderScenarioReference = x }

cardsUnderAgendaDeckL :: Lens' ScenarioAttrs [Card]
cardsUnderAgendaDeckL = lens scenarioCardsUnderAgendaDeck
  $ \m x -> m { scenarioCardsUnderAgendaDeck = x }

cardsUnderActDeckL :: Lens' ScenarioAttrs [Card]
cardsUnderActDeckL =
  lens scenarioCardsUnderActDeck $ \m x -> m { scenarioCardsUnderActDeck = x }

cardsNextToActDeckL :: Lens' ScenarioAttrs [Card]
cardsNextToActDeckL =
  lens scenarioCardsNextToActDeck $ \m x -> m { scenarioCardsNextToActDeck = x }

actStackL :: Lens' ScenarioAttrs (IntMap [Card])
actStackL = lens scenarioActStack $ \m x -> m { scenarioActStack = x }

agendaStackL :: Lens' ScenarioAttrs (IntMap [Card])
agendaStackL = lens scenarioAgendaStack $ \m x -> m { scenarioAgendaStack = x }

completedAgendaStackL :: Lens' ScenarioAttrs (IntMap [Card])
completedAgendaStackL = lens scenarioCompletedAgendaStack
  $ \m x -> m { scenarioCompletedAgendaStack = x }

completedActStackL :: Lens' ScenarioAttrs (IntMap [Card])
completedActStackL = lens scenarioCompletedActStack
  $ \m x -> m { scenarioCompletedActStack = x }

locationLayoutL :: Lens' ScenarioAttrs [GridTemplateRow]
locationLayoutL =
  lens scenarioLocationLayout $ \m x -> m { scenarioLocationLayout = x }

decksLayoutL :: Lens' ScenarioAttrs [GridTemplateRow]
decksLayoutL =
  lens scenarioDecksLayout $ \m x -> m { scenarioDecksLayout = x }

decksL :: Lens' ScenarioAttrs (HashMap ScenarioDeckKey [Card])
decksL = lens scenarioDecks $ \m x -> m { scenarioDecks = x }

logL :: Lens' ScenarioAttrs (HashSet ScenarioLogKey)
logL = lens scenarioLog $ \m x -> m { scenarioLog = x }

countsL :: Lens' ScenarioAttrs (HashMap ScenarioCountKey Int)
countsL = lens scenarioCounts $ \m x -> m { scenarioCounts = x }

standaloneCampaignLogL :: Lens' ScenarioAttrs CampaignLog
standaloneCampaignLogL = lens scenarioStandaloneCampaignLog
  $ \m x -> m { scenarioStandaloneCampaignLog = x }

setAsideCardsL :: Lens' ScenarioAttrs [Card]
setAsideCardsL =
  lens scenarioSetAsideCards $ \m x -> m { scenarioSetAsideCards = x }

inResolutionL :: Lens' ScenarioAttrs Bool
inResolutionL =
  lens scenarioInResolution $ \m x -> m { scenarioInResolution = x }

noRemainingInvestigatorsHandlerL :: Lens' ScenarioAttrs Target
noRemainingInvestigatorsHandlerL = lens scenarioNoRemainingInvestigatorsHandler
  $ \m x -> m { scenarioNoRemainingInvestigatorsHandler = x }

victoryDisplayL :: Lens' ScenarioAttrs [Card]
victoryDisplayL =
  lens scenarioVictoryDisplay $ \m x -> m { scenarioVictoryDisplay = x }

chaosBagL :: Lens' ScenarioAttrs ChaosBag
chaosBagL = lens scenarioChaosBag $ \m x -> m { scenarioChaosBag = x }

encounterDeckL :: Lens' ScenarioAttrs (Deck EncounterCard)
encounterDeckL =
  lens scenarioEncounterDeck $ \m x -> m { scenarioEncounterDeck = x }

discardL :: Lens' ScenarioAttrs [EncounterCard]
discardL = lens scenarioDiscard $ \m x -> m { scenarioDiscard = x }

resignedCardCodesL :: Lens' ScenarioAttrs [CardCode]
resignedCardCodesL =
  lens scenarioResignedCardCodes $ \m x -> m { scenarioResignedCardCodes = x }

storyCardsL :: Lens' ScenarioAttrs (HashMap InvestigatorId [PlayerCard])
storyCardsL = lens scenarioStoryCards $ \m x -> m { scenarioStoryCards = x }

data Scenario = forall a. IsScenario a => Scenario a

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

instance HasModifiersFor Scenario where
  getModifiersFor target (Scenario a) = getModifiersFor target a

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

