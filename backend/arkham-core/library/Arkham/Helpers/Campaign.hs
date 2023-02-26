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

completedScenario :: HasGame m => ScenarioId -> m Bool
completedScenario cCode = elem cCode <$> getCompletedScenarios

getCompletedScenarios :: HasGame m => m (HashSet ScenarioId)
getCompletedScenarios = do
  mcampaignId <- selectOne TheCampaign
  case mcampaignId of
    Nothing -> pure mempty
    Just campaignId -> do
      completedSteps <- field CampaignCompletedSteps campaignId
      pure . setFromList $ flip mapMaybe completedSteps $ \case
        ScenarioStep scenarioId -> Just scenarioId
        _ -> Nothing

getOwner :: (HasGame m, HasCardDef a) => a -> m (Maybe InvestigatorId)
getOwner cardDef = do
  campaignStoryCards <- getCampaignStoryCards
  pure $ findKey (any ((== toCardDef cardDef) . toCardDef)) campaignStoryCards

getCampaignStoryCards :: HasGame m => m (HashMap InvestigatorId [PlayerCard])
getCampaignStoryCards = do
  mCampaignId <- selectOne TheCampaign
  case mCampaignId of
    Just campaignId -> field CampaignStoryCards campaignId
    Nothing -> scenarioField ScenarioStoryCards

getCampaignStoryCard :: (HasCallStack, HasGame m, HasCardDef a) => a -> m PlayerCard
getCampaignStoryCard def = do
  cards <- concat . HashMap.elems <$> getCampaignStoryCards
  pure . fromJustNote "missing card" $ find ((== toCardDef def) . toCardDef) cards

getIsAlreadyOwned :: (HasGame m, HasCardDef a) => a -> m Bool
getIsAlreadyOwned cDef = do
  campaignStoryCards <- getCampaignStoryCards
  pure $ any ((== toCardDef cDef) . toCardDef) $ concat (toList campaignStoryCards)
