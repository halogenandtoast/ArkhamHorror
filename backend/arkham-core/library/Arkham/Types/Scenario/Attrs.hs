module Arkham.Types.Scenario.Attrs
  ( module Arkham.Types.Scenario.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.PlayerCard
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Decks
import Arkham.Types.Difficulty
import Arkham.Types.Game.Helpers
import Arkham.Types.Helpers
import Arkham.Types.Id
import Arkham.Types.Location as X
import Arkham.Types.Matcher hiding
  (ChosenRandomLocation, InvestigatorDefeated, InvestigatorEliminated)
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Deck as X
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window
import Control.Monad.Writer hiding (filterM)
import qualified Data.List.NonEmpty as NE

class IsScenario a

newtype GridTemplateRow = GridTemplateRow { unGridTemplateRow :: Text }
  deriving newtype (Show, IsString, ToJSON, FromJSON, Eq)

data ScenarioAttrs = ScenarioAttrs
  { scenarioName :: Name
  , scenarioId :: ScenarioId
  , scenarioDifficulty :: Difficulty
  -- These types are to handle complex scenarios with multiple stacks
  , scenarioAgendaStack :: [(Int, [CardDef])] -- These types are to handle complex scenarios with multiple stacks
  , scenarioCardsUnderScenarioReference :: [Card]
  , scenarioCardsUnderAgendaDeck :: [Card]
  , scenarioCardsUnderActDeck :: [Card]
  , scenarioActStack :: [(Int, [CardDef])]
  , scenarioLocationLayout :: Maybe [GridTemplateRow]
  , scenarioDeck :: Maybe ScenarioDeck
  , scenarioLog :: HashSet ScenarioLogKey
  , scenarioSetAsideCards :: [Card]
  , scenarioInResolution :: Bool
  }
  deriving stock (Show, Generic, Eq)

cardsUnderneathAgendaDeckL :: Lens' ScenarioAttrs [Card]
cardsUnderneathAgendaDeckL = lens scenarioCardsUnderAgendaDeck
  $ \m x -> m { scenarioCardsUnderAgendaDeck = x }

cardsUnderneathActDeckL :: Lens' ScenarioAttrs [Card]
cardsUnderneathActDeckL =
  lens scenarioCardsUnderActDeck $ \m x -> m { scenarioCardsUnderActDeck = x }

locationLayoutL :: Lens' ScenarioAttrs (Maybe [GridTemplateRow])
locationLayoutL =
  lens scenarioLocationLayout $ \m x -> m { scenarioLocationLayout = x }

inResolutionL :: Lens' ScenarioAttrs Bool
inResolutionL =
  lens scenarioInResolution $ \m x -> m { scenarioInResolution = x }

deckL :: Lens' ScenarioAttrs (Maybe ScenarioDeck)
deckL = lens scenarioDeck $ \m x -> m { scenarioDeck = x }

actStackL :: Lens' ScenarioAttrs [(Int, [CardDef])]
actStackL = lens scenarioActStack $ \m x -> m { scenarioActStack = x }

logL :: Lens' ScenarioAttrs (HashSet ScenarioLogKey)
logL = lens scenarioLog $ \m x -> m { scenarioLog = x }

setAsideCardsL :: Lens' ScenarioAttrs [Card]
setAsideCardsL =
  lens scenarioSetAsideCards $ \m x -> m { scenarioSetAsideCards = x }

cardsUnderScenarioReferenceL :: Lens' ScenarioAttrs [Card]
cardsUnderScenarioReferenceL = lens scenarioCardsUnderScenarioReference
  $ \m x -> m { scenarioCardsUnderScenarioReference = x }

instance ToJSON ScenarioAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "scenario"
  toEncoding = genericToEncoding $ aesonOptions $ Just "scenario"

instance FromJSON ScenarioAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "scenario"

instance HasRecord ScenarioAttrs where
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance HasName env ScenarioAttrs where
  getName = pure . scenarioName

instance HasCount ScenarioDeckCount env ScenarioAttrs where
  getCount ScenarioAttrs { scenarioDeck } = case scenarioDeck of
    Just (CultistDeck cards) -> pure . ScenarioDeckCount $ length cards
    Just (ExhibitDeck cards) -> pure . ScenarioDeckCount $ length cards
    Just (PotentialSacrifices cards) -> pure . ScenarioDeckCount $ length cards
    Nothing -> pure $ ScenarioDeckCount 0

instance HasCount SetAsideCount env (ScenarioAttrs, CardCode) where
  getCount (attrs, cardCode) = pure . SetAsideCount $ count
    ((== cardCode) . toCardCode)
    (attrs ^. setAsideCardsL)

instance HasList SetAsideCard env ScenarioAttrs where
  getList = pure . map SetAsideCard . scenarioSetAsideCards

instance HasList UnderScenarioReferenceCard env ScenarioAttrs where
  getList =
    pure . map UnderScenarioReferenceCard . scenarioCardsUnderScenarioReference

instance HasList UnderneathCard env (ScenarioAttrs, ActDeck) where
  getList (s, _) = pure $ map UnderneathCard (scenarioCardsUnderActDeck s)

instance HasList UnderneathCard env (ScenarioAttrs, AgendaDeck) where
  getList (s, _) = pure $ map UnderneathCard (scenarioCardsUnderAgendaDeck s)

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

baseAttrs
  :: CardCode -> Name -> [CardDef] -> [CardDef] -> Difficulty -> ScenarioAttrs
baseAttrs cardCode name agendaStack actStack' difficulty = ScenarioAttrs
  { scenarioId = ScenarioId cardCode
  , scenarioName = name
  , scenarioDifficulty = difficulty
  , scenarioAgendaStack = [(1, agendaStack)]
  , scenarioActStack = [(1, actStack')]
  , scenarioCardsUnderAgendaDeck = mempty
  , scenarioCardsUnderActDeck = mempty
  , scenarioLocationLayout = Nothing
  , scenarioDeck = Nothing
  , scenarioLog = mempty
  , scenarioSetAsideCards = mempty
  , scenarioCardsUnderScenarioReference = mempty
  , scenarioInResolution = False
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

instance HasTokenValue env InvestigatorId => HasTokenValue env ScenarioAttrs where
  getTokenValue _ iid = \case
    ElderSign -> getTokenValue iid iid ElderSign
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

type ScenarioAttrsRunner env
  = ( HasSet InScenarioInvestigatorId env ()
    , HasSet InvestigatorId env ()
    , HasSet AgendaId env ()
    , HasId (Maybe CampaignId) env ()
    , HasSet LocationId env ()
    , HasId LeadInvestigatorId env ()
    , Query LocationMatcher env
    , HasQueue env
    )

getIsStandalone
  :: (MonadReader env m, HasId (Maybe CampaignId) env ()) => m Bool
getIsStandalone = isNothing <$> getId @(Maybe CampaignId) ()

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

instance ScenarioAttrsRunner env => RunMessage env ScenarioAttrs where
  runMessage msg a@ScenarioAttrs {..} = case msg of
    Setup -> a <$ pushEnd BeginInvestigation
    StartCampaign -> do
      standalone <- getIsStandalone
      a <$ when standalone (push $ StartScenario scenarioName scenarioId)
    InitDeck iid deck -> do
      standalone <- getIsStandalone
      a <$ when
        standalone
        do
          (deck', randomWeaknesses) <- addRandomBasicWeaknessIfNeeded deck
          weaknesses <- traverse genPlayerCard randomWeaknesses
          push $ LoadDeck iid (Deck $ unDeck deck' <> weaknesses)
    PlaceLocationMatching cardMatcher -> do
      let
        matches = filter
          (`cardMatch` (CardWithType LocationType <> cardMatcher))
          scenarioSetAsideCards
      a <$ case matches of
        [] -> error "There were no locations with that name"
        (card : _) -> push (PlaceLocation card)
    EnemySpawnAtLocationMatching miid locationMatcher eid -> do
      lids <- selectList locationMatcher
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ case lids of
        [] -> push (Discard (EnemyTarget eid))
        [lid] -> pushAll (resolve $ EnemySpawn miid lid eid)
        xs -> spawnAtOneOf (fromMaybe leadInvestigatorId miid) eid xs
    PlaceDoomOnAgenda -> do
      agendaIds <- getSetList @AgendaId ()
      case agendaIds of
        [] -> pure a
        [x] -> a <$ push (PlaceDoom (AgendaTarget x) 1)
        _ -> error "multiple agendas should be handled by the scenario"
    Discard (ActTarget _) -> pure $ a & actStackL .~ []
    -- See: Vengeance Awaits / The Devourer Below - right now the assumption
    -- is that the act deck has been replaced.
    InvestigatorDefeated _ _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      if null investigatorIds && not scenarioInResolution
        then do
          clearQueue
          push (ScenarioResolution NoResolution)
          pure $ a & inResolutionL .~ True -- must set to avoid redundancy when scenario kills investigator
        else pure a
    AllInvestigatorsResigned -> a <$ push (ScenarioResolution NoResolution)
    InvestigatorWhenEliminated _ iid -> do
      whenMsgs <- checkWindows
        [Window Timing.When (Window.InvestigatorEliminated iid)]
      afterMsgs <- checkWindows
        [Window Timing.When (Window.InvestigatorEliminated iid)]
      a <$ pushAll
        (whenMsgs
        <> [ InvestigatorPlaceAllCluesOnLocation iid
           , InvestigatorEliminated iid
           ]
        <> afterMsgs
        )
    Remember logKey -> pure $ a & logL %~ insertSet logKey
    ResolveToken _drawnToken token _iid | token == AutoFail ->
      a <$ push FailSkillTest
    EndOfScenario -> do
      clearQueue
      standalone <- getIsStandalone
      a <$ push (if standalone then GameOver else NextCampaignStep Nothing)
    ScenarioResolution _ ->
      error "The scenario should specify what to do for no resolution"
    UseScenarioSpecificAbility{} ->
      error
        "The scenario should specify what to do for a scenario specific ability."
    LookAtTopOfDeck _ ScenarioDeckTarget _ ->
      error "The scenario should handle looking at the top of the scenario deck"
    ChooseRandomLocation target exclusions -> do
      locationIds <-
        setToList . (`difference` exclusions) <$> getSet @LocationId ()
      leadInvestigatorId <- getLeadInvestigatorId
      case locationIds of
        [] -> error "no locations?"
        (h : t) -> do
          randomLocationId <- sample $ h :| t
          a <$ pushAll
            [ CheckWindow
              leadInvestigatorId
              [ Window
                  Timing.When
                  (Window.ChosenRandomLocation randomLocationId)
              ]
            , ChosenRandomLocation target randomLocationId
            ]
    PlaceLocation card -> pure $ a & setAsideCardsL %~ delete card
    AddToEncounterDeck card -> do
      pure $ a & setAsideCardsL %~ deleteFirstMatch (== EncounterCard card)
    CreateStoryAssetAt card _ -> do
      pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
    AttachStoryTreacheryTo card _ -> do
      pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
    CreateEnemyAt card _ _ -> do
      pure $ a & setAsideCardsL %~ deleteFirstMatch (== card)
    PlaceUnderneath AgendaDeckTarget cards -> do
      push (After msg)
      pure $ a & cardsUnderneathAgendaDeckL <>~ cards
    PlaceUnderneath ActDeckTarget cards -> do
      push (After msg)
      pure $ a & cardsUnderneathActDeckL <>~ cards
    RequestSetAsideCard target cardCode -> do
      let
        (before, rest) =
          break ((== cardCode) . toCardCode) scenarioSetAsideCards
      case rest of
        [] -> error "requested a card that is not set aside"
        (x : xs) -> do
          push (RequestedSetAsideCard target x)
          pure $ a & setAsideCardsL .~ (before <> xs)
    ReadStory card -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push
        (chooseOne
          leadInvestigatorId
          [CardLabel (toCardCode card) [ResolveStory card]]
        )
      pure $ a & cardsUnderScenarioReferenceL %~ filter (/= card)
    _ -> pure a
