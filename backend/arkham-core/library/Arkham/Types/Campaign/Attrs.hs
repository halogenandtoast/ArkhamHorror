module Arkham.Types.Campaign.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignLog
import Arkham.Types.CampaignLogKey
import Arkham.Types.CampaignStep
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Game.Helpers
import Arkham.Types.Helpers
import Arkham.Types.Id
import Arkham.Types.Investigator
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Resolution
import Arkham.Types.Token

class IsCampaign a

data CampaignAttrs = CampaignAttrs
  { campaignId :: CampaignId
  , campaignName :: Text
  , campaignInvestigators :: Map Int Investigator
  , campaignDecks :: Map InvestigatorId (Deck PlayerCard)
  , campaignStoryCards :: Map InvestigatorId [PlayerCard]
  , campaignDifficulty :: Difficulty
  , campaignChaosBag :: [TokenFace]
  , campaignLog :: CampaignLog
  , campaignStep :: Maybe CampaignStep
  , campaignCompletedSteps :: [CampaignStep]
  , campaignResolutions :: Map ScenarioId Resolution
  }
  deriving stock (Show, Generic, Eq)

completedStepsL :: Lens' CampaignAttrs [CampaignStep]
completedStepsL =
  lens campaignCompletedSteps $ \m x -> m { campaignCompletedSteps = x }

chaosBagL :: Lens' CampaignAttrs [TokenFace]
chaosBagL = lens campaignChaosBag $ \m x -> m { campaignChaosBag = x }

storyCardsL :: Lens' CampaignAttrs (Map InvestigatorId [PlayerCard])
storyCardsL = lens campaignStoryCards $ \m x -> m { campaignStoryCards = x }

decksL :: Lens' CampaignAttrs (Map InvestigatorId (Deck PlayerCard))
decksL = lens campaignDecks $ \m x -> m { campaignDecks = x }

logL :: Lens' CampaignAttrs CampaignLog
logL = lens campaignLog $ \m x -> m { campaignLog = x }

stepL :: Lens' CampaignAttrs (Maybe CampaignStep)
stepL = lens campaignStep $ \m x -> m { campaignStep = x }

resolutionsL :: Lens' CampaignAttrs (Map ScenarioId Resolution)
resolutionsL = lens campaignResolutions $ \m x -> m { campaignResolutions = x }

completeStep :: Maybe CampaignStep -> [CampaignStep] -> [CampaignStep]
completeStep (Just step') steps = step' : steps
completeStep Nothing steps = steps

instance Entity CampaignAttrs where
  type EntityId CampaignAttrs = CampaignId
  type EntityAttrs CampaignAttrs = CampaignAttrs
  toId = campaignId
  toAttrs = id

instance Named CampaignAttrs where
  toName = mkName . campaignName

instance ToJSON CampaignAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "campaign"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaign"

instance FromJSON CampaignAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaign"

instance HasSet CompletedScenarioId env CampaignAttrs where
  getSet CampaignAttrs {..} =
    pure . setFromList $ flip mapMaybe campaignCompletedSteps $ \case
      ScenarioStep scenarioId -> Just $ CompletedScenarioId scenarioId
      _ -> Nothing

instance HasList CampaignStoryCard env CampaignAttrs where
  getList CampaignAttrs {..} =
    pure $ concatMap (uncurry (map . CampaignStoryCard)) $ mapToList
      campaignStoryCards

instance CampaignRunner env => RunMessage env CampaignAttrs where
  runMessage msg a@CampaignAttrs {..} = case msg of
    StartCampaign -> a <$ push (CampaignStep campaignStep)
    CampaignStep Nothing -> a <$ push GameOver -- TODO: move to generic
    CampaignStep (Just (ScenarioStep sid)) -> do
      scenarioName <- getName sid
      a <$ pushAll [ResetGame, StartScenario scenarioName sid]
    CampaignStep (Just (UpgradeDeckStep _)) -> do
      investigatorIds <- getInvestigatorIds
      a <$ pushAll
        (ResetGame
        : map chooseUpgradeDeck investigatorIds
        <> [FinishedUpgradingDecks]
        )
    SetTokensForScenario -> a <$ push (SetTokens campaignChaosBag)
    AddCampaignCardToDeck iid cardDef -> do
      card <- lookupPlayerCard cardDef <$> getRandom
      pure $ a & storyCardsL %~ insertWith (<>) iid [card]
    AddCampaignCardToEncounterDeck cardDef -> do
      card <- lookupEncounterCard cardDef <$> getRandom
      a <$ pushAll [AddToEncounterDeck card]
    RemoveCampaignCardFromDeck iid cardCode ->
      pure
        $ a
        & storyCardsL
        %~ adjustMap (filter ((/= cardCode) . cdCardCode . toCardDef)) iid
    AddToken token -> pure $ a & chaosBagL %~ (token :)
    InitDeck iid deck -> pure $ a & decksL %~ insertMap iid deck
    UpgradeDeck iid deck -> pure $ a & decksL %~ insertMap iid deck
    FinishedUpgradingDecks -> case a ^. stepL of
      Just (UpgradeDeckStep nextStep) -> do
        push (CampaignStep $ Just nextStep)
        pure $ a & stepL ?~ nextStep
      _ -> error "invalid state"
    ResetGame -> do
      for_ (mapToList campaignDecks) $ \(iid, deck) -> do
        let investigatorStoryCards = findWithDefault [] iid campaignStoryCards
        push (LoadDeck iid . Deck $ unDeck deck <> investigatorStoryCards)
      pure a
    CrossOutRecord key ->
      pure
        $ a
        & (logL . recorded %~ deleteSet key)
        & (logL . recordedSets %~ deleteMap key)
        & (logL . recordedCounts %~ deleteMap key)
    Record key -> pure $ a & logL . recorded %~ insertSet key
    RecordSet key cardCodes ->
      pure $ a & logL . recordedSets %~ insertMap key cardCodes
    RecordCount key int ->
      pure $ a & logL . recordedCounts %~ insertMap key int
    ScenarioResolution r -> case campaignStep of
      Just (ScenarioStep sid) -> pure $ a & resolutionsL %~ insertMap sid r
      _ -> error "must be called in a scenario"
    DrivenInsane iid -> pure $ a & logL . recordedSets %~ insertWith
      (<>)
      DrivenInsaneInvestigators
      (singleton $ unInvestigatorId iid)
    _ -> pure a

baseAttrs :: CampaignId -> Text -> Difficulty -> [TokenFace] -> CampaignAttrs
baseAttrs campaignId' name difficulty chaosBagContents = CampaignAttrs
  { campaignId = campaignId'
  , campaignName = name
  , campaignInvestigators = mempty
  , campaignDecks = mempty
  , campaignStoryCards = mempty
  , campaignDifficulty = difficulty
  , campaignChaosBag = chaosBagContents
  , campaignLog = mkCampaignLog
  , campaignStep = Just PrologueStep
  , campaignCompletedSteps = []
  , campaignResolutions = mempty
  }
