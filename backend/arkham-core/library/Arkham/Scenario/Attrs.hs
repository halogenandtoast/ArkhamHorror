module Arkham.Scenario.Attrs
  ( module Arkham.Scenario.Attrs
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

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasModifiersFor a, RunMessage a, HasTokenValue a, Entity a, EntityId a ~ ScenarioId, EntityAttrs a ~ ScenarioAttrs) => IsScenario a

newtype GridTemplateRow = GridTemplateRow { unGridTemplateRow :: Text }
  deriving newtype (Show, IsString, ToJSON, FromJSON, Eq)

data instance Field ScenarioAttrs :: Type -> Type where
  ScenarioCardsUnderActDeck :: Field ScenarioAttrs [Card]
  ScenarioCardsUnderAgendaDeck :: Field ScenarioAttrs [Card]
  ScenarioCardsUnderScenarioReference :: Field ScenarioAttrs [Card]
  ScenarioDiscard :: Field ScenarioAttrs [EncounterCard]
  ScenarioEncounterDeck :: Field ScenarioAttrs (Deck EncounterCard)
  ScenarioDifficulty :: Field ScenarioAttrs Difficulty
  ScenarioDecks :: Field ScenarioAttrs (HashMap ScenarioDeckKey [Card])
  ScenarioVictoryDisplay :: Field ScenarioAttrs [Card]
  ScenarioRemembered :: Field ScenarioAttrs (HashSet ScenarioLogKey)
  ScenarioStandaloneCampaignLog :: Field ScenarioAttrs CampaignLog
  ScenarioResignedCardCodes :: Field ScenarioAttrs [CardCode]
  ScenarioChaosBag :: Field ScenarioAttrs ChaosBag
  ScenarioSetAsideCards :: Field ScenarioAttrs [Card]
  ScenarioName :: Field ScenarioAttrs Name
  ScenarioStoryCards :: Field ScenarioAttrs (HashMap InvestigatorId [PlayerCard])

data ScenarioAttrs = ScenarioAttrs
  { scenarioName :: Name
  , scenarioId :: ScenarioId
  , scenarioDifficulty :: Difficulty
  , scenarioCardsUnderScenarioReference :: [Card]
  , scenarioCardsUnderAgendaDeck :: [Card]
  , scenarioCardsUnderActDeck :: [Card]
  , scenarioCardsNextToActDeck :: [Card]
  , scenarioActStack :: IntMap [CardDef]
  , scenarioAgendaStack :: IntMap [CardDef]
  , scenarioCompletedAgendaStack :: IntMap [CardDef]
  , scenarioLocationLayout :: Maybe [GridTemplateRow]
  , scenarioDecks :: HashMap ScenarioDeckKey [Card]
  , scenarioLog :: HashSet ScenarioLogKey
  , scenarioStandaloneCampaignLog :: CampaignLog
  , scenarioSetAsideCards :: [Card]
  , scenarioInResolution :: Bool
  , scenarioNoRemainingInvestigatorsHandler :: Target
  , scenarioVictoryDisplay :: [Card]
  , scenarioChaosBag :: ChaosBag
  , scenarioEncounterDeck :: Deck EncounterCard
  , scenarioDiscard :: [EncounterCard]
  , scenarioResignedCardCodes :: [CardCode]
  -- for standalone
  , scenarioStoryCards :: HashMap InvestigatorId [PlayerCard]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ScenarioAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "scenario"

instance FromJSON ScenarioAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "scenario"

baseAttrs :: CardCode -> Name -> Difficulty -> ScenarioAttrs
baseAttrs cardCode name difficulty = ScenarioAttrs
  { scenarioId = ScenarioId cardCode
  , scenarioName = name
  , scenarioDifficulty = difficulty
  , scenarioCompletedAgendaStack = mempty
  , scenarioAgendaStack = mempty
  , scenarioActStack = mempty
  , scenarioCardsUnderAgendaDeck = mempty
  , scenarioCardsUnderActDeck = mempty
  , scenarioCardsNextToActDeck = mempty
  , scenarioLocationLayout = Nothing
  , scenarioDecks = mempty
  , scenarioLog = mempty
  , scenarioSetAsideCards = mempty
  , scenarioStandaloneCampaignLog = mkCampaignLog
  , scenarioCardsUnderScenarioReference = mempty
  , scenarioInResolution = False
  , scenarioNoRemainingInvestigatorsHandler = ScenarioTarget
    (ScenarioId cardCode)
  , scenarioVictoryDisplay = mempty
  , scenarioChaosBag = emptyChaosBag
  , scenarioEncounterDeck = mempty
  , scenarioDiscard = mempty
  , scenarioResignedCardCodes = mempty
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

instance TargetEntity ScenarioAttrs where
  toTarget = ScenarioTarget . toId
  isTarget ScenarioAttrs { scenarioId } (ScenarioTarget sid) =
    scenarioId == sid
  isTarget _ _ = False

instance SourceEntity ScenarioAttrs where
  toSource = ScenarioSource . toId
  isSource ScenarioAttrs { scenarioId } (ScenarioSource sid) =
    scenarioId == sid
  isSource _ _ = False

nameL :: Lens' ScenarioAttrs Name
nameL = lens scenarioName $ \m x -> m { scenarioName = x }

idL :: Lens' ScenarioAttrs ScenarioId
idL = lens scenarioId $ \m x -> m { scenarioId = x }

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

actStackL :: Lens' ScenarioAttrs (IntMap [CardDef])
actStackL = lens scenarioActStack $ \m x -> m { scenarioActStack = x }

agendaStackL :: Lens' ScenarioAttrs (IntMap [CardDef])
agendaStackL = lens scenarioAgendaStack $ \m x -> m { scenarioAgendaStack = x }

completedAgendaStackL :: Lens' ScenarioAttrs (IntMap [CardDef])
completedAgendaStackL = lens scenarioCompletedAgendaStack
  $ \m x -> m { scenarioCompletedAgendaStack = x }

locationLayoutL :: Lens' ScenarioAttrs (Maybe [GridTemplateRow])
locationLayoutL =
  lens scenarioLocationLayout $ \m x -> m { scenarioLocationLayout = x }

decksL :: Lens' ScenarioAttrs (HashMap ScenarioDeckKey [Card])
decksL = lens scenarioDecks $ \m x -> m { scenarioDecks = x }

logL :: Lens' ScenarioAttrs (HashSet ScenarioLogKey)
logL = lens scenarioLog $ \m x -> m { scenarioLog = x }

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

