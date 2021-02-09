{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Scenario.Attrs
  ( module Arkham.Types.Scenario.Attrs
  , module X
  )
where


import Arkham.Types.ScenarioLogKey
import Arkham.Types.Difficulty
import Arkham.Types.Scenario.Deck as X
import Arkham.Types.Scenario.Runner
import Arkham.Types.Location as X

newtype GridTemplateRow = GridTemplateRow { unGridTemplateRow :: Text }
  deriving newtype (Show, IsString, ToJSON, FromJSON, Eq)

data ScenarioAttrs = ScenarioAttrs
  { scenarioName :: Text
  , scenarioId :: ScenarioId
  , scenarioDifficulty :: Difficulty
  -- These types are to handle complex scenarios with multiple stacks
  , scenarioAgendaStack :: [(Int, [AgendaId])] -- These types are to handle complex scenarios with multiple stacks
  , scenarioActStack :: [(Int, [ActId])]
  , scenarioLocationLayout :: Maybe [GridTemplateRow]
  , scenarioDeck :: Maybe ScenarioDeck
  , scenarioLog :: HashSet ScenarioLogKey
  , scenarioLocations :: HashMap LocationName [LocationId]
  , scenarioSetAsideCards :: [Card]
  }
  deriving stock (Show, Generic, Eq)

makeLensesWith suffixedFields ''ScenarioAttrs

instance ToJSON ScenarioAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "scenario"
  toEncoding = genericToEncoding $ aesonOptions $ Just "scenario"

instance FromJSON ScenarioAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "scenario"

instance HasCount ScenarioDeckCount env ScenarioAttrs where
  getCount ScenarioAttrs { scenarioDeck } = case scenarioDeck of
    Just (CultistDeck cards) -> pure . ScenarioDeckCount $ length cards
    Just (ExhibitDeck cards) -> pure . ScenarioDeckCount $ length cards
    Just (PotentialSacrifices cards) -> pure . ScenarioDeckCount $ length cards
    Nothing -> pure $ ScenarioDeckCount 0

toTokenValue :: ScenarioAttrs -> Token -> Int -> Int -> TokenValue
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
  :: CardCode -> Text -> [AgendaId] -> [ActId] -> Difficulty -> ScenarioAttrs
baseAttrs cardCode name agendaStack actStack' difficulty = ScenarioAttrs
  { scenarioId = ScenarioId cardCode
  , scenarioName = name
  , scenarioDifficulty = difficulty
  , scenarioAgendaStack = [(1, agendaStack)]
  , scenarioActStack = [(1, actStack')]
  , scenarioLocationLayout = Nothing
  , scenarioDeck = Nothing
  , scenarioLog = mempty
  , scenarioLocations = mempty
  , scenarioSetAsideCards = mempty
  }

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = ScenarioId
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs = id

instance NamedEntity ScenarioAttrs where
  toName = mkName . scenarioName

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
  :: LocationMatcher -> HashMap LocationName [LocationId] -> Maybe LocationName
findLocationKey locationMatcher locations = find matchKey $ keys locations
 where
  matchKey (LocationName (Name title msubtitle)) = case locationMatcher of
    LocationWithTitle title' -> title == title'
    LocationWithFullTitle title' subtitle' ->
      title == title' && Just subtitle' == msubtitle


instance ScenarioRunner env => RunMessage env ScenarioAttrs where
  runMessage msg a@ScenarioAttrs {..} = case msg of
    Setup -> a <$ pushMessage BeginInvestigation
    StartCampaign -> do
      standalone <- isNothing <$> getId @(Maybe CampaignId) ()
      a <$ if standalone
        then unshiftMessage $ StartScenario scenarioId
        else pure ()
    InitDeck iid deck -> do
      standalone <- isNothing <$> getId @(Maybe CampaignId) ()
      a <$ if standalone then unshiftMessage $ LoadDeck iid deck else pure ()
    PlaceLocationMatching locationMatcher -> do
      let
        locations =
          fromMaybe []
            $ findLocationKey locationMatcher scenarioLocations
            >>= flip lookup scenarioLocations
      a <$ case locations of
        [] -> error "There were no locations with that name"
        [lid] -> unshiftMessage (PlaceLocation lid)
        _ ->
          error "We want there to be only one location when targetting names"
    EnemySpawnAtLocationMatching miid locationMatcher eid -> do
      let
        locations =
          fromMaybe []
            $ findLocationKey locationMatcher scenarioLocations
            >>= flip lookup scenarioLocations
      a <$ case locations of
        [] -> error "There were no locations with that name"
        [lid] -> unshiftMessage (EnemySpawn miid lid eid)
        _ ->
          error "We want there to be only one location when targetting names"
    PlaceDoomOnAgenda -> do
      agendaIds <- getSetList @AgendaId ()
      case agendaIds of
        [] -> pure a
        [x] -> a <$ unshiftMessage (PlaceDoom (AgendaTarget x) 1)
        _ -> error "multiple agendas should be handled by the scenario"
    Discard (ActTarget _) -> pure $ a & actStackL .~ []
    -- ^ See: Vengeance Awaits / The Devourer Below - right now the assumption
    -- | is that the act deck has been replaced.
    InvestigatorDefeated _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      if null investigatorIds
        then do
          clearQueue
          a <$ unshiftMessage (ScenarioResolution NoResolution)
        else pure a
    AllInvestigatorsResigned ->
      a <$ unshiftMessage (ScenarioResolution NoResolution)
    InvestigatorWhenEliminated iid ->
      a <$ unshiftMessage (InvestigatorEliminated iid)
    Remember logKey -> pure $ a & logL %~ insertSet logKey
    ResolveToken _drawnToken token _iid | token == AutoFail ->
      a <$ unshiftMessage FailSkillTest
    EndOfScenario -> do
      clearQueue
      standalone <- isNothing <$> getId @(Maybe CampaignId) ()
      a <$ unshiftMessage
        (if standalone then GameOver else NextCampaignStep Nothing)
    ScenarioResolution _ ->
      error "The scenario should specify what to do for no resolution"
    UseScenarioSpecificAbility{} ->
      error
        "The scenario should specify what to do for a scenario specific ability."
    LookAtTopOfDeck _ ScenarioDeckTarget _ ->
      error "The scenario should handle looking at the top of the scenario deck"
    _ -> pure a
