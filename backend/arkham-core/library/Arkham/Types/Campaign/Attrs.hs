{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Campaign.Attrs where


import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignLog
import Arkham.Types.CampaignStep
import Arkham.Types.Difficulty
import Arkham.Types.Game.Helpers
import Arkham.Types.Investigator

data CampaignAttrs = CampaignAttrs
  { campaignId :: CampaignId
  , campaignName :: Text
  , campaignInvestigators :: HashMap Int Investigator
  , campaignDecks :: HashMap InvestigatorId [PlayerCard]
  , campaignStoryCards :: HashMap InvestigatorId [PlayerCard]
  , campaignDifficulty :: Difficulty
  , campaignChaosBag :: [Token]
  , campaignLog :: CampaignLog
  , campaignStep :: Maybe CampaignStep
  , campaignCompletedSteps :: [CampaignStep]
  , campaignResolutions :: HashMap ScenarioId Resolution
  }
  deriving stock (Show, Generic, Eq)

makeLensesWith suffixedFields ''CampaignAttrs

completeStep :: Maybe CampaignStep -> [CampaignStep] -> [CampaignStep]
completeStep (Just step') steps = step' : steps
completeStep Nothing steps = steps

instance Entity CampaignAttrs where
  type EntityId CampaignAttrs = CampaignId
  type EntityAttrs CampaignAttrs = CampaignAttrs
  toId = campaignId
  toAttrs = id

instance NamedEntity CampaignAttrs where
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
    StartCampaign -> a <$ unshiftMessage (CampaignStep campaignStep)
    CampaignStep Nothing -> a <$ unshiftMessage GameOver -- TODO: move to generic
    CampaignStep (Just (ScenarioStep sid)) ->
      a <$ unshiftMessages [ResetGame, StartScenario sid]
    CampaignStep (Just (UpgradeDeckStep _)) -> do
      investigatorIds <- getInvestigatorIds
      a <$ unshiftMessages
        (ResetGame
        : map chooseUpgradeDeck investigatorIds
        <> [FinishedUpgradingDecks]
        )
    SetTokensForScenario -> a <$ unshiftMessage (SetTokens campaignChaosBag)
    AddCampaignCardToDeck iid cardCode -> do
      card <- lookupPlayerCard cardCode <$> getRandom
      pure $ a & storyCardsL %~ insertWith (<>) iid [card]
    AddCampaignCardToEncounterDeck cardCode -> do
      card <- lookupEncounterCard cardCode <$> getRandom
      a <$ unshiftMessages [AddToEncounterDeck card]
    RemoveCampaignCardFromDeck iid cardCode ->
      pure
        $ a
        & storyCardsL
        %~ adjustMap (filter ((/= cardCode) . pcCardCode)) iid
    AddToken token -> pure $ a & chaosBagL %~ (token :)
    InitDeck iid deck -> pure $ a & decksL %~ insertMap iid deck
    UpgradeDeck iid deck -> pure $ a & decksL %~ insertMap iid deck
    FinishedUpgradingDecks -> case a ^. stepL of
      Just (UpgradeDeckStep nextStep) -> do
        unshiftMessage (CampaignStep $ Just nextStep)
        pure $ a & stepL ?~ nextStep
      _ -> error "invalid state"
    ResetGame -> do
      for_ (mapToList campaignDecks) $ \(iid, deck) -> do
        let investigatorStoryCards = findWithDefault [] iid campaignStoryCards
        unshiftMessage (LoadDeck iid $ deck <> investigatorStoryCards)
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
    _ -> pure a

baseAttrs :: CampaignId -> Text -> Difficulty -> [Token] -> CampaignAttrs
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
