module Arkham.Helpers.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Data.Aeson (Result (..))
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
        . setFromList
        $ flip mapMaybe completedSteps
        $ \case
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

getCampaignMeta :: forall a m. (HasGame m, FromJSON a) => m a
getCampaignMeta = do
  result <- fromJSON @a <$> campaignField CampaignMeta
  case result of
    Success a -> pure a
    Error e -> error $ "Failed to parse campaign meta: " <> e

matchingCardsAlreadyInDeck
  :: HasGame m => CardMatcher -> m (Map InvestigatorId (Set CardCode))
matchingCardsAlreadyInDeck matcher = do
  decks <- campaignField CampaignDecks
  pure
    $ Map.map
      (setFromList . map toCardCode . filter (`cardMatch` matcher) . unDeck)
      decks

addCampaignCardToDeckChoice
  :: PlayerId -> [InvestigatorId] -> CardDef -> Message
addCampaignCardToDeckChoice leadPlayer investigators cardDef =
  questionLabelWithCard
    ("Add " <> display name <> " to a deck")
    (toCardCode cardDef)
    leadPlayer
    $ ChooseOne
    $ [ PortraitLabel investigator [AddCampaignCardToDeck investigator cardDef]
      | investigator <- investigators
      ]
    <> [Label ("Do not add " <> display name <> " to any deck") []]
 where
  name = cdName cardDef

forceAddCampaignCardToDeckChoice
  :: PlayerId -> [InvestigatorId] -> CardDef -> Message
forceAddCampaignCardToDeckChoice _ [onlyId] cardDef =
  AddCampaignCardToDeck
    onlyId
    cardDef
forceAddCampaignCardToDeckChoice leadPlayer investigators cardDef =
  questionLabelWithCard
    ("Add " <> display name <> " to a deck")
    (toCardCode cardDef)
    leadPlayer
    $ ChooseOne
      [ PortraitLabel investigator [AddCampaignCardToDeck investigator cardDef]
      | investigator <- investigators
      ]
 where
  name = cdName cardDef
