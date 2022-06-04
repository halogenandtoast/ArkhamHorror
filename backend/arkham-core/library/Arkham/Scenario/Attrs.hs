{-# LANGUAGE TemplateHaskell #-}
module Arkham.Scenario.Attrs
  ( module Arkham.Scenario.Attrs
  , module X
  , Field(..)
  ) where

import Arkham.Prelude

import Data.Aeson.TH
import Arkham.Card
import Arkham.ChaosBag
import Arkham.Classes.HasRecord
import Arkham.Classes.HasTokenValue
import Arkham.Classes.Entity
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.Id
import Arkham.Json
import Arkham.Matcher hiding
  ( ChosenRandomLocation
  , InvestigatorDefeated
  , InvestigatorEliminated
  , PlaceUnderneath
  )
import Arkham.Name
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Scenario.Deck as X
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Control.Monad.Writer hiding (filterM)
import Data.List.NonEmpty qualified as NE

class IsScenario a

newtype GridTemplateRow = GridTemplateRow { unGridTemplateRow :: Text }
  deriving newtype (Show, IsString, ToJSON, FromJSON, Eq)

data instance Field ScenarioAttrs :: Type -> Type where
  ScenarioCardsUnderActDeck :: Field ScenarioAttrs [Card]
  ScenarioCardsUnderAgendaDeck :: Field ScenarioAttrs [Card]

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
  , scenarioSetAsideCards :: [Card]
  , scenarioInResolution :: Bool
  , scenarioNoRemainingInvestigatorsHandler :: Target
  , scenarioVictoryDisplay :: [Card]
  , scenarioChaosBag :: ChaosBag
  , scenarioEncounterDeck :: Deck EncounterCard
  , scenarioDiscard :: [EncounterCard]
  -- for standalone
  , scenarioStoryCards :: HashMap InvestigatorId [PlayerCard]
  }
  deriving stock (Show, Eq)

cardsUnderneathAgendaDeckL :: Lens' ScenarioAttrs [Card]
cardsUnderneathAgendaDeckL = lens scenarioCardsUnderAgendaDeck
  $ \m x -> m { scenarioCardsUnderAgendaDeck = x }

cardsUnderneathActDeckL :: Lens' ScenarioAttrs [Card]
cardsUnderneathActDeckL =
  lens scenarioCardsUnderActDeck $ \m x -> m { scenarioCardsUnderActDeck = x }

cardsNextToActDeckL :: Lens' ScenarioAttrs [Card]
cardsNextToActDeckL =
  lens scenarioCardsNextToActDeck $ \m x -> m { scenarioCardsNextToActDeck = x }

locationLayoutL :: Lens' ScenarioAttrs (Maybe [GridTemplateRow])
locationLayoutL =
  lens scenarioLocationLayout $ \m x -> m { scenarioLocationLayout = x }

inResolutionL :: Lens' ScenarioAttrs Bool
inResolutionL =
  lens scenarioInResolution $ \m x -> m { scenarioInResolution = x }

noRemainingInvestigatorsHandlerL :: Lens' ScenarioAttrs Target
noRemainingInvestigatorsHandlerL = lens scenarioNoRemainingInvestigatorsHandler
  $ \m x -> m { scenarioNoRemainingInvestigatorsHandler = x }

decksL :: Lens' ScenarioAttrs (HashMap ScenarioDeckKey [Card])
decksL = lens scenarioDecks $ \m x -> m { scenarioDecks = x }

actStackL :: Lens' ScenarioAttrs (IntMap [CardDef])
actStackL = lens scenarioActStack $ \m x -> m { scenarioActStack = x }

agendaStackL :: Lens' ScenarioAttrs (IntMap [CardDef])
agendaStackL = lens scenarioAgendaStack $ \m x -> m { scenarioAgendaStack = x }

completedAgendaStackL :: Lens' ScenarioAttrs (IntMap [CardDef])
completedAgendaStackL = lens scenarioCompletedAgendaStack
  $ \m x -> m { scenarioCompletedAgendaStack = x }

logL :: Lens' ScenarioAttrs (HashSet ScenarioLogKey)
logL = lens scenarioLog $ \m x -> m { scenarioLog = x }

setAsideCardsL :: Lens' ScenarioAttrs [Card]
setAsideCardsL =
  lens scenarioSetAsideCards $ \m x -> m { scenarioSetAsideCards = x }

cardsUnderScenarioReferenceL :: Lens' ScenarioAttrs [Card]
cardsUnderScenarioReferenceL = lens scenarioCardsUnderScenarioReference
  $ \m x -> m { scenarioCardsUnderScenarioReference = x }

storyCardsL :: Lens' ScenarioAttrs (HashMap InvestigatorId [PlayerCard])
storyCardsL = lens scenarioStoryCards $ \m x -> m { scenarioStoryCards = x }

$(deriveJSON (aesonOptions $ Just "Scenario") ''ScenarioAttrs)

instance Monad m => HasRecord m ScenarioAttrs where
  hasRecord _ _ = pure False
  hasRecordSet _ _ = pure []
  hasRecordCount _ _ = pure 0

toTokenValue :: ScenarioAttrs -> TokenFace -> Int -> Int -> TokenValue
toTokenValue attrs t esVal heVal = TokenValue
  t
  (NegativeModifier $ if isEasyStandard attrs then esVal else heVal)

isEasyStandard :: ScenarioAttrs -> Bool
isEasyStandard ScenarioAttrs { scenarioDifficulty } =
  scenarioDifficulty `elem` [Easy, Standard]

isHardExpert :: ScenarioAttrs -> Bool
isHardExpert ScenarioAttrs { scenarioDifficulty } =
  scenarioDifficulty `elem` [Hard, Expert]

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
  , scenarioCardsUnderScenarioReference = mempty
  , scenarioInResolution = False
  , scenarioNoRemainingInvestigatorsHandler = ScenarioTarget
    (ScenarioId cardCode)
  , scenarioVictoryDisplay = mempty
  , scenarioChaosBag = emptyChaosBag
  , scenarioEncounterDeck = mempty
  , scenarioDiscard = mempty
  , scenarioStoryCards = mempty
  }

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = ScenarioId
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs = id

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

instance (Monad m, HasTokenValue m InvestigatorId) => HasTokenValue m ScenarioAttrs where
  getTokenValue iid tokenFace _ = case tokenFace of
    ElderSign -> getTokenValue iid ElderSign iid
    AutoFail -> pure $ TokenValue AutoFail AutoFailModifier
    PlusOne -> pure $ TokenValue PlusOne (PositiveModifier 1)
    Zero -> pure $ TokenValue Zero (PositiveModifier 0)
    MinusOne -> pure $ TokenValue MinusOne (NegativeModifier 1)
    MinusTwo -> pure $ TokenValue MinusTwo (NegativeModifier 2)
    MinusThree -> pure $ TokenValue MinusThree (NegativeModifier 3)
    MinusFour -> pure $ TokenValue MinusFour (NegativeModifier 4)
    MinusFive -> pure $ TokenValue MinusFive (NegativeModifier 5)
    MinusSix -> pure $ TokenValue MinusSix (NegativeModifier 6)
    MinusSeven -> pure $ TokenValue MinusSeven (NegativeModifier 7)
    MinusEight -> pure $ TokenValue MinusEight (NegativeModifier 8)
    otherFace -> pure $ TokenValue otherFace NoModifier

getIsStandalone
  :: (MonadReader env m, Query CampaignMatcher m) => m Bool
getIsStandalone = isNothing <$> selectOne TheCampaign

addRandomBasicWeaknessIfNeeded
  :: MonadRandom m => Deck PlayerCard -> m (Deck PlayerCard, [CardDef])
addRandomBasicWeaknessIfNeeded deck = runWriterT $ do
  Deck <$> flip
    filterM
    (unDeck deck)
    \card -> do
      when
        (toCardDef card == randomWeakness)
        (sample (NE.fromList allBasicWeaknesses) >>= tell . pure)
      pure $ toCardDef card /= randomWeakness
