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
getOwner cardDef = findKey (any ((== cardDef) . toCardDef)) <$> getCampaignStoryCards

withOwner :: HasGame m => CardDef -> (InvestigatorId -> m ()) -> m ()
withOwner cardDef f =
  getOwner cardDef >>= \case
    Nothing -> pure ()
    Just iid -> f iid

getCampaignStoryCards :: HasGame m => m (Map InvestigatorId [PlayerCard])
getCampaignStoryCards = do
  mCampaignId <- selectOne TheCampaign
  case mCampaignId of
    Just campaignId -> field CampaignStoryCards campaignId
    Nothing -> scenarioField ScenarioStoryCards

getCampaignStoryCard :: (HasCallStack, HasGame m) => CardDef -> m PlayerCard
getCampaignStoryCard def = fromJustNote "missing card" <$> getMaybeCampaignStoryCard def

getMaybeCampaignStoryCard :: (HasCallStack, HasGame m) => CardDef -> m (Maybe PlayerCard)
getMaybeCampaignStoryCard def = do
  cards <- concat . Map.elems <$> getCampaignStoryCards
  pure $ find ((== def) . toCardDef) cards

getIsAlreadyOwned :: HasGame m => CardDef -> m Bool
getIsAlreadyOwned cDef = any (any ((== cDef) . toCardDef)) . toList <$> getCampaignStoryCards

campaignField :: (HasCallStack, HasGame m) => Field Campaign a -> m a
campaignField fld = selectJust TheCampaign >>= field fld

getCampaignMeta :: forall a m. (HasCallStack, HasGame m, FromJSON a) => m a
getCampaignMeta = do
  result <- fromJSON @a <$> campaignField CampaignMeta
  case result of
    Success a -> pure a
    Error e -> error $ "Failed to parse campaign meta: " <> e

getCampaignStore :: HasGame m => m (Map Text Value)
getCampaignStore = campaignField CampaignStore

stored :: forall a m. (HasCallStack, HasGame m, FromJSON a) => Text -> m (Maybe a)
stored k = do
  store <- getCampaignStore
  pure $ case lookup k store of
    Nothing -> Nothing
    Just v -> case fromJSON v of
      Success a -> Just a
      Error e -> error $ "Failed to parse stored value: " <> e

matchingCardsAlreadyInDeck
  :: HasGame m => CardMatcher -> m (Map InvestigatorId (Set CardCode))
matchingCardsAlreadyInDeck matcher = do
  decks <- campaignField CampaignDecks
  pure $ Map.map (setFromList . map toCardCode . filter (`cardMatch` matcher) . unDeck) decks

addCampaignCardToDeckChoice
  :: PlayerId -> [InvestigatorId] -> Card -> Message
addCampaignCardToDeckChoice leadPlayer investigators card =
  questionLabelWithCard ("Add " <> display card.name <> " to a deck") card.cardCode leadPlayer
    $ ChooseOne
    $ [ PortraitLabel investigator [AddCampaignCardToDeck investigator card]
      | investigator <- investigators
      ]
    <> [Label ("Do not add " <> display card.name <> " to any deck") []]

forceAddCampaignCardToDeckChoice
  :: PlayerId -> [InvestigatorId] -> Card -> Message
forceAddCampaignCardToDeckChoice _ [onlyId] card = AddCampaignCardToDeck onlyId card
forceAddCampaignCardToDeckChoice leadPlayer investigators card =
  questionLabelWithCard ("Add " <> display card.name <> " to a deck") card.cardCode leadPlayer
    $ ChooseOne
      [ PortraitLabel investigator [AddCampaignCardToDeck investigator card]
      | investigator <- investigators
      ]
