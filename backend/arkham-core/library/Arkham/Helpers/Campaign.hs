module Arkham.Helpers.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.CampaignStep
import Arkham.Card.CardDef
import Arkham.Card.PlayerCard
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Types ( Field (..) )
import Data.HashMap.Strict qualified as HashMap

completedScenario :: (Monad m, HasGame m) => ScenarioId -> m Bool
completedScenario cCode = elem cCode <$> getCompletedScenarios

getCompletedScenarios :: (Monad m, HasGame m) => m (HashSet ScenarioId)
getCompletedScenarios = do
  mcampaignId <- selectOne TheCampaign
  case mcampaignId of
    Nothing -> pure mempty
    Just campaignId -> do
      completedSteps <- field CampaignCompletedSteps campaignId
      pure . setFromList $ flip mapMaybe completedSteps $ \case
        ScenarioStep scenarioId -> Just scenarioId
        _ -> Nothing

getOwner :: (Monad m, HasGame m) => CardDef -> m (Maybe InvestigatorId)
getOwner cardDef = do
  campaignStoryCards <- getCampaignStoryCards
  pure $ findKey (any ((== cardDef) . toCardDef)) campaignStoryCards

getCampaignStoryCards
  :: (Monad m, HasGame m) => m (HashMap InvestigatorId [PlayerCard])
getCampaignStoryCards = do
  mCampaignId <- selectOne TheCampaign
  case mCampaignId of
    Just campaignId -> field CampaignStoryCards campaignId
    Nothing -> scenarioField ScenarioStoryCards

getCampaignStoryCard
  :: (HasCallStack, Monad m, HasGame m) => CardDef -> m PlayerCard
getCampaignStoryCard def = do
  cards <- concat . HashMap.elems <$> getCampaignStoryCards
  pure . fromJustNote "missing card" $ find ((== def) . toCardDef) cards

getIsAlreadyOwned :: (Monad m, HasGame m) => CardDef -> m Bool
getIsAlreadyOwned cDef = do
  campaignStoryCards <- getCampaignStoryCards
  pure $ any ((== cDef) . toCardDef) $ concat (toList campaignStoryCards)
