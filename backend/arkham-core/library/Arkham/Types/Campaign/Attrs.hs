{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign.Attrs where

import Arkham.Import hiding (log)

import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignLog
import Arkham.Types.CampaignStep
import Arkham.Types.Difficulty
import Arkham.Types.Investigator
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.UUID.V4

instance HasAttrs Attrs where
  type AttrsT Attrs = Attrs
  toAttrs = id

data Attrs = Attrs
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
  }
  deriving stock (Show, Generic)

chaosBag :: Lens' Attrs [Token]
chaosBag = lens campaignChaosBag $ \m x -> m { campaignChaosBag = x }

decks :: Lens' Attrs (HashMap InvestigatorId [PlayerCard])
decks = lens campaignDecks $ \m x -> m { campaignDecks = x }

storyCards :: Lens' Attrs (HashMap InvestigatorId [PlayerCard])
storyCards = lens campaignStoryCards $ \m x -> m { campaignStoryCards = x }

step :: Lens' Attrs (Maybe CampaignStep)
step = lens campaignStep $ \m x -> m { campaignStep = x }

completedSteps :: Lens' Attrs [CampaignStep]
completedSteps =
  lens campaignCompletedSteps $ \m x -> m { campaignCompletedSteps = x }

log :: Lens' Attrs CampaignLog
log = lens campaignLog $ \m x -> m { campaignLog = x }

completeStep :: Maybe CampaignStep -> [CampaignStep] -> [CampaignStep]
completeStep (Just step') steps = step' : steps
completeStep Nothing steps = steps

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "campaign"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaign"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaign"

instance HasSet CompletedScenarioId env Attrs where
  getSet Attrs {..} =
    pure . setFromList $ flip mapMaybe campaignCompletedSteps $ \case
      ScenarioStep scenarioId -> Just $ CompletedScenarioId scenarioId
      _ -> Nothing

instance (CampaignRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    StartCampaign -> a <$ unshiftMessage (CampaignStep campaignStep)
    CampaignStep Nothing -> a <$ unshiftMessage GameOver -- TODO: move to generic
    CampaignStep (Just (ScenarioStep sid)) ->
      a <$ unshiftMessages [ResetGame, StartScenario sid]
    SetTokensForScenario -> a <$ unshiftMessage (SetTokens campaignChaosBag)
    AddCampaignCardToDeck iid cardCode -> do
      card <- liftIO $ lookupPlayerCard cardCode . CardId <$> nextRandom
      pure $ a & storyCards %~ HashMap.insertWith (<>) iid [card]
    AddCampaignCardToEncounterDeck cardCode -> do
      card <- liftIO $ lookupEncounterCard cardCode . CardId <$> nextRandom
      a <$ unshiftMessages [AddToEncounterDeck card]
    RemoveCampaignCardFromDeck iid cardCode ->
      pure
        $ a
        & storyCards
        %~ HashMap.adjust (filter ((/= cardCode) . pcCardCode)) iid
    AddToken token -> pure $ a & chaosBag %~ (token :)
    InitDeck iid deck -> pure $ a & decks %~ HashMap.insert iid deck
    ResetGame -> do
      for_ (HashMap.toList campaignDecks) $ \(iid, deck) -> do
        let
          investigatorStoryCards =
            HashMap.findWithDefault [] iid campaignStoryCards
        unshiftMessage (LoadDeck iid $ deck <> investigatorStoryCards)
      pure a
    CrossOutRecord key ->
      pure
        $ a
        & (log . recorded %~ HashSet.delete key)
        & (log . recordedSets %~ HashMap.delete key)
        & (log . recordedCounts %~ HashMap.delete key)
    Record key -> pure $ a & log . recorded %~ HashSet.insert key
    RecordSet key cardCodes ->
      pure $ a & log . recordedSets %~ HashMap.insert key cardCodes
    RecordCount key int ->
      pure $ a & log . recordedCounts %~ HashMap.insert key int
    _ -> pure a

baseAttrs :: CampaignId -> Text -> Difficulty -> [Token] -> Attrs
baseAttrs campaignId' name difficulty chaosBagContents = Attrs
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
  }
