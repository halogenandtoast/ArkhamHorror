module Arkham.Helpers.Campaign where

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
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Tracing
import Data.Aeson (Result (..))
import Data.Map.Strict qualified as Map

completedScenario :: (HasGame m, Tracing m) => ScenarioId -> m Bool
completedScenario cCode = elem cCode <$> getCompletedScenarios

getCompletedScenarios :: (HasGame m, Tracing m) => m (Set ScenarioId)
getCompletedScenarios = setFromList <$> getCompletedScenariosList

getCompletedSteps :: (HasGame m, Tracing m) => m [CampaignStep]
getCompletedSteps =
  selectOne TheCampaign >>= \case
    Nothing -> pure mempty
    Just campaignId -> field CampaignCompletedSteps campaignId

getCompletedScenariosList :: (HasGame m, Tracing m) => m [ScenarioId]
getCompletedScenariosList = do
  selectOne TheCampaign >>= \case
    Nothing -> pure mempty
    Just campaignId -> do
      completedSteps <- field CampaignCompletedSteps campaignId
      pure
        $ flip mapMaybe completedSteps
        $ \case
          ScenarioStep scenarioId -> Just scenarioId
          _ -> Nothing

getOwner :: (HasGame m, Tracing m) => CardDef -> m (Maybe InvestigatorId)
getOwner cardDef = do
  iids <- select $ IncludeEliminated Anyone
  cardMap <- getCampaignStoryCards
  let inGame = Map.filterWithKey (\k _ -> k `elem` iids) cardMap
  pure $ findKey (any ((== cardDef) . toCardDef)) inGame

withOwner :: (HasGame m, Tracing m) => CardDef -> (InvestigatorId -> m ()) -> m ()
withOwner cardDef f =
  getOwner cardDef >>= \case
    Nothing -> pure ()
    Just iid -> f iid

getCampaignStoryCards :: (HasGame m, Tracing m) => m (Map InvestigatorId [Card])
getCampaignStoryCards = do
  mCampaignId <- selectOne TheCampaign
  case mCampaignId of
    Just campaignId -> field CampaignStoryCards campaignId
    Nothing -> scenarioField ScenarioStoryCards

getCampaignStoryCard :: (HasCallStack, HasGame m, Tracing m) => CardDef -> m Card
getCampaignStoryCard def = fromJustNote "missing card" <$> getMaybeCampaignStoryCard def

getMaybeCampaignStoryCard :: (HasGame m, Tracing m, HasCardCode def) => def -> m (Maybe Card)
getMaybeCampaignStoryCard (toCardCode -> cardCode) = do
  cards <- concat . Map.elems <$> getCampaignStoryCards
  pure $ find ((== toCardCode cardCode) . toCardCode) cards

getIsAlreadyOwned :: (HasGame m, Tracing m) => CardDef -> m Bool
getIsAlreadyOwned cDef = any (any ((== cDef) . toCardDef)) . toList <$> getCampaignStoryCards

campaignField :: (HasCallStack, HasGame m, Tracing m) => Field Campaign a -> m a
campaignField fld = selectJust TheCampaign >>= field fld

getCampaignMeta :: forall a m. (HasCallStack, HasGame m, Tracing m, FromJSON a) => m a
getCampaignMeta = do
  result <- fromJSON @a <$> campaignField CampaignMeta
  case result of
    Success a -> pure a
    Error e -> error $ "Failed to parse campaign meta: " <> e

withCampaignMeta
  :: forall a m r. (HasCallStack, HasGame m, Tracing m, FromJSON a) => (a -> r) -> m r
withCampaignMeta f = f <$> getCampaignMeta @a

getCampaignStore :: (HasCallStack, HasGame m, Tracing m) => m (Map Text Value)
getCampaignStore = campaignField CampaignStore

stored :: forall a m. (HasCallStack, HasGame m, Tracing m, FromJSON a) => Text -> m (Maybe a)
stored k = do
  store <- getCampaignStore
  pure $ case lookup k store of
    Nothing -> Nothing
    Just v -> case fromJSON v of
      Success a -> Just a
      Error e -> error $ "Failed to parse stored value: " <> e

matchingCardsAlreadyInDeck
  :: (HasGame m, Tracing m) => CardMatcher -> m (Map InvestigatorId (Set CardCode))
matchingCardsAlreadyInDeck matcher = do
  decks <- campaignField CampaignDecks
  pure $ Map.map (setFromList . map toCardCode . filter (`cardMatch` matcher) . unDeck) decks

addCampaignCardToDeckChoice
  :: PlayerId -> [InvestigatorId] -> ShuffleIn -> Card -> Message
addCampaignCardToDeckChoice leadPlayer investigators shouldShuffleIn card =
  addCampaignCardToDeckChoiceWith leadPlayer investigators shouldShuffleIn card (const [])

addCampaignCardToDeckChoiceWith
  :: PlayerId -> [InvestigatorId] -> ShuffleIn -> Card -> (InvestigatorId -> [Message]) -> Message
addCampaignCardToDeckChoiceWith leadPlayer investigators shouldShuffleIn card f =
  questionLabelWithCard ("Add " <> display card.name <> " to a deck") card.cardCode leadPlayer
    $ ChooseOne
    $ [ PortraitLabel investigator $ AddCampaignCardToDeck investigator shouldShuffleIn card
          : f investigator
      | investigator <- investigators
      ]
    <> [Label ("Do not add " <> display card.name <> " to any deck") []]

forceAddCampaignCardToDeckChoice
  :: PlayerId -> [InvestigatorId] -> ShuffleIn -> Card -> Message
forceAddCampaignCardToDeckChoice _ [onlyId] shouldShuffleIn card = AddCampaignCardToDeck onlyId shouldShuffleIn card
forceAddCampaignCardToDeckChoice leadPlayer investigators shouldShuffleIn card =
  questionLabelWithCard ("Add " <> display card.name <> " to a deck") card.cardCode leadPlayer
    $ ChooseOne
      [ PortraitLabel investigator [AddCampaignCardToDeck investigator shouldShuffleIn card]
      | investigator <- investigators
      ]
