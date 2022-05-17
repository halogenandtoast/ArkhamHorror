{-# LANGUAGE TemplateHaskell #-}
module Arkham.Campaign.Attrs where

import Arkham.Prelude

import Data.Aeson.TH
import Arkham.Json
import Arkham.PlayerCard
import Arkham.Campaign.Runner
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Game.Helpers
import Arkham.Helpers
import Arkham.Id
import Arkham.Investigator
import Arkham.Message
import Arkham.Name
import Arkham.Resolution
import Arkham.Token
import Control.Monad.Writer hiding (filterM)
import Data.List.NonEmpty qualified as NE

class IsCampaign a

data CampaignAttrs = CampaignAttrs
  { campaignId :: CampaignId
  , campaignName :: Text
  , campaignInvestigators :: HashMap Int Investigator
  , campaignDecks :: HashMap InvestigatorId (Deck PlayerCard)
  , campaignStoryCards :: HashMap InvestigatorId [PlayerCard]
  , campaignDifficulty :: Difficulty
  , campaignChaosBag :: [TokenFace]
  , campaignLog :: CampaignLog
  , campaignStep :: Maybe CampaignStep
  , campaignCompletedSteps :: [CampaignStep]
  , campaignResolutions :: HashMap ScenarioId Resolution
  }
  deriving stock (Show, Eq)

completedStepsL :: Lens' CampaignAttrs [CampaignStep]
completedStepsL =
  lens campaignCompletedSteps $ \m x -> m { campaignCompletedSteps = x }

chaosBagL :: Lens' CampaignAttrs [TokenFace]
chaosBagL = lens campaignChaosBag $ \m x -> m { campaignChaosBag = x }

storyCardsL :: Lens' CampaignAttrs (HashMap InvestigatorId [PlayerCard])
storyCardsL = lens campaignStoryCards $ \m x -> m { campaignStoryCards = x }

decksL :: Lens' CampaignAttrs (HashMap InvestigatorId (Deck PlayerCard))
decksL = lens campaignDecks $ \m x -> m { campaignDecks = x }

logL :: Lens' CampaignAttrs CampaignLog
logL = lens campaignLog $ \m x -> m { campaignLog = x }

stepL :: Lens' CampaignAttrs (Maybe CampaignStep)
stepL = lens campaignStep $ \m x -> m { campaignStep = x }

resolutionsL :: Lens' CampaignAttrs (HashMap ScenarioId Resolution)
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

$(deriveJSON defaultOptions ''CampaignAttrs)

instance HasSet CompletedScenarioId env CampaignAttrs where
  getSet CampaignAttrs {..} =
    pure . setFromList $ flip mapMaybe campaignCompletedSteps $ \case
      ScenarioStep scenarioId -> Just $ CompletedScenarioId scenarioId
      _ -> Nothing

instance HasList CampaignStoryCard env CampaignAttrs where
  getList CampaignAttrs {..} =
    pure $ concatMap (uncurry (map . CampaignStoryCard)) $ mapToList
      campaignStoryCards

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
      pure $ a & storyCardsL %~ insertWith
        (<>)
        iid
        [card { pcBearer = Just iid }]
    RemoveCampaignCardFromDeck iid cardCode ->
      pure
        $ a
        & storyCardsL
        %~ adjustMap (filter ((/= cardCode) . cdCardCode . toCardDef)) iid
    AddToken token -> pure $ a & chaosBagL %~ (token :)
    RemoveAllTokens token -> pure $ a & chaosBagL %~ filter (/= token)
    InitDeck iid deck -> do
      (deck', randomWeaknesses) <- addRandomBasicWeaknessIfNeeded deck
      pushAll $ map (AddCampaignCardToDeck iid) randomWeaknesses
      pure $ a & decksL %~ insertMap iid deck'
    UpgradeDeck iid deck -> do
      -- We remove the random weakness if the upgrade deck still has it listed
      -- since this will have been added at the beginning of the campaign
      (deck', _) <- addRandomBasicWeaknessIfNeeded deck
      pure $ a & decksL %~ insertMap iid deck'
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
    CrossOutRecord key -> do
      let
        crossedOutModifier =
          if key `member` view (logL . recorded) a then insertSet key else id

      pure
        $ a
        & (logL . recorded %~ deleteSet key)
        & (logL . crossedOut %~ crossedOutModifier)
        & (logL . recordedSets %~ deleteMap key)
        & (logL . recordedCounts %~ deleteMap key)
    Record key -> pure $ a & logL . recorded %~ insertSet key
    RecordSet key cardCodes ->
      pure $ a & logL . recordedSets %~ insertMap key (map Recorded cardCodes)
    RecordSetInsert key cardCodes ->
      pure $ case a ^. logL . recordedSets . at key of
        Nothing ->
          a & logL . recordedSets %~ insertMap key (map Recorded cardCodes)
        Just set ->
          let
            set' =
              filter ((`notElem` cardCodes) . unrecorded) set
                <> map Recorded cardCodes
          in a & logL . recordedSets %~ insertMap key set'
    CrossOutRecordSetEntries key cardCodes ->
      pure
        $ a
        & logL
        . recordedSets
        %~ adjustMap
             (map
               (\case
                 Recorded c | c `elem` cardCodes -> CrossedOut c
                 other -> other
               )
             )
             key
    RecordCount key int ->
      pure $ a & logL . recordedCounts %~ insertMap key int
    ScenarioResolution r -> case campaignStep of
      Just (ScenarioStep sid) -> pure $ a & resolutionsL %~ insertMap sid r
      _ -> error "must be called in a scenario"
    DrivenInsane iid -> pure $ a & logL . recordedSets %~ insertWith
      (<>)
      DrivenInsaneInvestigators
      (singleton $ Recorded $ unInvestigatorId iid)
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
