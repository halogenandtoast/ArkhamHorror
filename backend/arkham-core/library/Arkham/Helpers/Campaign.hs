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
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Data.Map.Strict qualified as Map

completedScenario :: HasGame m => ScenarioId -> m Bool
completedScenario cCode = elem cCode <$> getCompletedScenarios

getCompletedScenarios :: HasGame m => m (Set ScenarioId)
getCompletedScenarios = do
  mcampaignId <- selectOne TheCampaign
  case mcampaignId of
    Nothing -> pure mempty
    Just campaignId -> do
      completedSteps <- field CampaignCompletedSteps campaignId
      pure
        . setFromList $ flip mapMaybe completedSteps $ \case
          ScenarioStep scenarioId -> Just scenarioId
          _ -> Nothing

getOwner :: HasGame m => CardDef -> m (Maybe InvestigatorId)
getOwner cardDef = do
  campaignStoryCards <- getCampaignStoryCards
  pure $ findKey (any ((== cardDef) . toCardDef)) campaignStoryCards

getCampaignStoryCards :: HasGame m => m (Map InvestigatorId [PlayerCard])
getCampaignStoryCards = do
  mCampaignId <- selectOne TheCampaign
  case mCampaignId of
    Just campaignId -> field CampaignStoryCards campaignId
    Nothing -> scenarioField ScenarioStoryCards

getCampaignStoryCard :: (HasCallStack, HasGame m) => CardDef -> m PlayerCard
getCampaignStoryCard def = do
  cards <- concat . Map.elems <$> getCampaignStoryCards
  pure . fromJustNote "missing card" $ find ((== def) . toCardDef) cards

getIsAlreadyOwned :: HasGame m => CardDef -> m Bool
getIsAlreadyOwned cDef = do
  campaignStoryCards <- getCampaignStoryCards
  pure $ any ((== cDef) . toCardDef) $ concat (toList campaignStoryCards)

campaignField :: (HasCallStack, HasGame m) => Field Campaign a -> m a
campaignField fld = selectJust TheCampaign >>= field fld

matchingCardsAlreadyInDeck
  :: HasGame m => CardMatcher -> m (Map InvestigatorId (Set CardCode))
matchingCardsAlreadyInDeck matcher = do
  decks <- campaignField CampaignDecks
  pure
    $ Map.map
      (setFromList . map toCardCode . filter (`cardMatch` matcher) . unDeck)
      decks

addCampaignCardToDeckChoice
  :: InvestigatorId -> [InvestigatorId] -> CardDef -> Message
addCampaignCardToDeckChoice leadInvestigatorId investigatorIds cardDef =
  questionLabelWithCard
    ("Add " <> display name <> " to a deck")
    (toCardCode cardDef)
    leadInvestigatorId
    $ ChooseOne
    $ [ PortraitLabel
        iid
        [AddCampaignCardToDeck iid cardDef]
      | iid <- investigatorIds
      ]
    <> [Label ("Do not add " <> display name <> " to any deck") []]
 where
  name = cdName cardDef

forceAddCampaignCardToDeckChoice
  :: InvestigatorId -> [InvestigatorId] -> CardDef -> Message
forceAddCampaignCardToDeckChoice _ [onlyId] cardDef =
  AddCampaignCardToDeck
    onlyId
    cardDef
forceAddCampaignCardToDeckChoice leadInvestigatorId investigatorIds cardDef =
  questionLabelWithCard
    ("Add " <> display name <> " to a deck")
    (toCardCode cardDef)
    leadInvestigatorId
    $ ChooseOne
    $ [ PortraitLabel
        iid
        [AddCampaignCardToDeck iid cardDef]
      | iid <- investigatorIds
      ]
 where
  name = cdName cardDef
