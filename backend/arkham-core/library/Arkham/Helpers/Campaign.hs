module Arkham.Helpers.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Store
import Data.Map.Strict qualified as Map

completedScenario :: (HasGame m, Store m Card) => ScenarioId -> m Bool
completedScenario cCode = elem cCode <$> getCompletedScenarios

getCompletedScenarios :: (HasGame m, Store m Card) => m (Set ScenarioId)
getCompletedScenarios = do
  mcampaignId <- selectOne TheCampaign
  case mcampaignId of
    Nothing -> pure mempty
    Just campaignId -> do
      completedSteps <- field CampaignCompletedSteps campaignId
      pure . setFromList $ flip mapMaybe completedSteps $ \case
        ScenarioStep scenarioId -> Just scenarioId
        _ -> Nothing

getOwner :: (HasGame m, Store m Card) => CardDef -> m (Maybe InvestigatorId)
getOwner cardDef = do
  campaignStoryCards <- getCampaignStoryCards
  pure $ findKey (any ((== cardDef) . toCardDef)) campaignStoryCards

getCampaignStoryCards :: (HasGame m, Store m Card) => m (Map InvestigatorId [PlayerCard])
getCampaignStoryCards = do
  mCampaignId <- selectOne TheCampaign
  case mCampaignId of
    Just campaignId -> field CampaignStoryCards campaignId
    Nothing -> scenarioField ScenarioStoryCards

getCampaignStoryCard :: (HasCallStack, HasGame m, Store m Card) => CardDef -> m PlayerCard
getCampaignStoryCard def = do
  cards <- concat . Map.elems <$> getCampaignStoryCards
  pure . fromJustNote "missing card" $ find ((== def) . toCardDef) cards

getIsAlreadyOwned :: (HasGame m, Store m Card) => CardDef -> m Bool
getIsAlreadyOwned cDef = do
  campaignStoryCards <- getCampaignStoryCards
  pure $ any ((== cDef) . toCardDef) $ concat (toList campaignStoryCards)

campaignField :: (HasCallStack, HasGame m, Store m Card) => Field Campaign a -> m a
campaignField fld = selectJust TheCampaign >>= field fld

matchingCardsAlreadyInDeck
  :: (HasGame m, Store m Card) => CardMatcher -> m (Map InvestigatorId (Set CardCode))
matchingCardsAlreadyInDeck matcher = do
  decks <- campaignField CampaignDecks
  pure $
    Map.map
      (setFromList . map toCardCode . filter (`cardMatch` matcher) . unDeck)
      decks
