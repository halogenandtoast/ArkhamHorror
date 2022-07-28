module Arkham.Helpers.Card where

import Arkham.Prelude

import Arkham.Card
import Arkham.Projection
import Arkham.Matcher
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Classes.Query
import Arkham.Id
import Arkham.Helpers.Scenario
import Arkham.Campaign.Types ( Field (..) )
import Arkham.Scenario.Types ( Field (..) )
import Data.HashMap.Strict qualified as HashMap

getCampaignStoryCards :: (Monad m, HasGame m) => m (HashMap InvestigatorId [PlayerCard])
getCampaignStoryCards = do
  mCampaignId <- selectOne TheCampaign
  case mCampaignId of
    Just campaignId -> field CampaignStoryCards campaignId
    Nothing -> scenarioField ScenarioStoryCards

getCampaignStoryCard :: (HasCallStack, Monad m, HasGame m) => CardDef -> m PlayerCard
getCampaignStoryCard def = do
  cards <- concat . HashMap.elems <$> getCampaignStoryCards
  pure . fromJustNote "missing card" $ find ((== def) . toCardDef) cards

isDiscardable :: Card -> Bool
isDiscardable = not . isWeakness
 where
  isWeakness = \case
    PlayerCard pc   -> isJust $ cdCardSubType $ toCardDef pc
    EncounterCard _ -> True -- maybe?
