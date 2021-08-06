module Arkham.Types.Scenario.Attrs
  ( module Arkham.Types.Scenario.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Decks
import Arkham.Types.Difficulty
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Location as X
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Deck as X
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Window

newtype GridTemplateRow = GridTemplateRow { unGridTemplateRow :: Text }
  deriving newtype (Show, IsString, ToJSON, FromJSON, Eq)

data ScenarioAttrs = ScenarioAttrs
  { scenarioName :: Name
  , scenarioId :: ScenarioId
  , scenarioDifficulty :: Difficulty
  -- These types are to handle complex scenarios with multiple stacks
  , scenarioAgendaStack :: [(Int, [AgendaId])] -- These types are to handle complex scenarios with multiple stacks
  , scenarioCardsUnderAgendaDeck :: [Card]
  , scenarioCardsUnderActDeck :: [Card]
  , scenarioActStack :: [(Int, [ActId])]
  , scenarioLocationLayout :: Maybe [GridTemplateRow]
  , scenarioDeck :: Maybe ScenarioDeck
  , scenarioLog :: Set ScenarioLogKey
  , scenarioLocations :: Map LocationName [CardDef]
  , scenarioSetAsideCards :: [Card]
  , scenarioInResolution :: Bool
  }
  deriving stock (Show, Generic, Eq)

locationNameMap :: [CardDef] -> Map LocationName [CardDef]
locationNameMap defs = unionsWith (<>) $ map toMap defs
  where toMap = liftM2 singletonMap (LocationName . toName) pure

cardsUnderneathAgendaDeckL :: Lens' ScenarioAttrs [Card]
cardsUnderneathAgendaDeckL = lens scenarioCardsUnderAgendaDeck
  $ \m x -> m { scenarioCardsUnderAgendaDeck = x }

cardsUnderneathActDeckL :: Lens' ScenarioAttrs [Card]
cardsUnderneathActDeckL =
  lens scenarioCardsUnderActDeck $ \m x -> m { scenarioCardsUnderActDeck = x }

locationsL :: Lens' ScenarioAttrs (Map LocationName [CardDef])
locationsL = lens scenarioLocations $ \m x -> m { scenarioLocations = x }

inResolutionL :: Lens' ScenarioAttrs Bool
inResolutionL =
  lens scenarioInResolution $ \m x -> m { scenarioInResolution = x }

deckL :: Lens' ScenarioAttrs (Maybe ScenarioDeck)
deckL = lens scenarioDeck $ \m x -> m { scenarioDeck = x }

actStackL :: Lens' ScenarioAttrs [(Int, [ActId])]
actStackL = lens scenarioActStack $ \m x -> m { scenarioActStack = x }

logL :: Lens' ScenarioAttrs (Set ScenarioLogKey)
logL = lens scenarioLog $ \m x -> m { scenarioLog = x }

setAsideCardsL :: Lens' ScenarioAttrs [Card]
setAsideCardsL =
  lens scenarioSetAsideCards $ \m x -> m { scenarioSetAsideCards = x }

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
  :: CardCode -> Name -> [AgendaId] -> [ActId] -> Difficulty -> ScenarioAttrs
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
  , scenarioLocations = mempty
  , scenarioSetAsideCards = mempty
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

findLocationKey
  :: LocationMatcher -> Map LocationName [CardDef] -> Maybe LocationName
findLocationKey locationMatcher locations = fst
  <$> find match (mapToList locations)
 where
  match (LocationName (Name title msubtitle), _) = case locationMatcher of
    LocationWithTitle title' -> title == title'
    LocationWithFullTitle title' subtitle' ->
      title == title' && Just subtitle' == msubtitle
    LocationWithLabel _ -> error "can not use label"
    LocationWithId _ -> error "can not use id"
    Anywhere -> True
    -- TODO: Encode these into an either?
    EmptyLocation -> error "needs to find a singular location"
    FarthestLocationFromYou _ -> error "needs to find a singular location"
    LocationMatchers _ -> error "not implemented"
    LocationWithTrait _ -> error "not implemented"
    LocationWithoutInvestigators -> error "needs to find a singular location"
    LocationWithoutEnemies -> error "needs to find a singular location"
    RevealedLocation -> error "needs to find a singular location"
    LocationWithoutTreachery _ -> error "needs to find a singular location"
    YourLocation -> error ":("
    AccessibleLocation -> error ":("
    ConnectedLocation -> error ":("
    LocationWithClues -> error ":("
    NotYourLocation -> error ":("
    LocationWithoutTreacheryWithCardCode _ -> error ":("
    FirstLocation _ -> error ":("

type ScenarioAttrsRunner env
  = ( HasSet InScenarioInvestigatorId env ()
    , HasSet AgendaId env ()
    , HasId (Maybe CampaignId) env ()
    , HasSet LocationId env ()
    , HasId LeadInvestigatorId env ()
    , HasQueue env
    )

getIsStandalone
  :: (MonadReader env m, HasId (Maybe CampaignId) env ()) => m Bool
getIsStandalone = isNothing <$> getId @(Maybe CampaignId) ()

instance (HasId (Maybe LocationId) env LocationMatcher, ScenarioAttrsRunner env) => RunMessage env ScenarioAttrs where
  runMessage msg a@ScenarioAttrs {..} = case msg of
    Setup -> a <$ pushEnd BeginInvestigation
    StartCampaign -> do
      standalone <- getIsStandalone
      a <$ if standalone
        then push $ StartScenario scenarioName scenarioId
        else pure ()
    InitDeck iid deck -> do
      standalone <- getIsStandalone
      a <$ if standalone then push $ LoadDeck iid deck else pure ()
    PlaceLocationMatching locationMatcher -> do
      mlid <- getId @(Maybe LocationId) locationMatcher
      case mlid of
        Just _ -> pure a
        Nothing -> do
          let
            locations =
              fromMaybe []
                $ findLocationKey locationMatcher scenarioLocations
                >>= flip lookup scenarioLocations
          a <$ case locations of
            [] -> error "There were no locations with that name"
            [cardDef] -> do
              lid <- getRandom
              push (PlaceLocation lid cardDef)
            _ -> error
              "We want there to be only one location when targetting names"
    EnemySpawnAtLocationMatching miid locationMatcher eid -> do
      mlid <- getId locationMatcher
      a <$ case mlid of
        Nothing -> push (Discard (EnemyTarget eid))
        Just lid -> push (EnemySpawn miid lid eid)
    PlaceDoomOnAgenda -> do
      agendaIds <- getSetList @AgendaId ()
      case agendaIds of
        [] -> pure a
        [x] -> a <$ push (PlaceDoom (AgendaTarget x) 1)
        _ -> error "multiple agendas should be handled by the scenario"
    Discard (ActTarget _) -> pure $ a & actStackL .~ []
    -- See: Vengeance Awaits / The Devourer Below - right now the assumption
    -- is that the act deck has been replaced.
    InvestigatorDefeated _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      if null investigatorIds && not scenarioInResolution
        then do
          clearQueue
          push (ScenarioResolution NoResolution)
          pure $ a & inResolutionL .~ True -- must set to avoid redundancy when scenario kills investigator
        else pure a
    AllInvestigatorsResigned -> a <$ push (ScenarioResolution NoResolution)
    InvestigatorWhenEliminated _ iid -> a <$ push (InvestigatorEliminated iid)
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
              [WhenChosenRandomLocation randomLocationId]
            , ChosenRandomLocation target randomLocationId
            ]
    PlaceLocation _ cardDef -> pure $ a & setAsideCardsL %~ deleteFirstMatch
      ((== toCardCode cardDef) . toCardCode)
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
    _ -> pure a
