module Arkham.Helpers.Card where

import Arkham.Prelude

import Arkham.Card
import Arkham.Projection
import Arkham.Matcher
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Classes.Query
import Arkham.Campaign.Attrs ( Field (..) )
import Arkham.Scenario.Attrs ( Field (..) )

getCampaignStoryCard :: CardDef -> GameT PlayerCard
getCampaignStoryCard def = do
  mCampaignId <- selectOne TheCampaign
  cards <- case mCampaignId of
    Just campaignId -> fieldMap CampaignStoryCards (concat . toList) campaignId
    Nothing -> scenarioFieldMap ScenarioStoryCards (concat . toList)
  pure . fromJustNote "missing card" $ find ((== def) . toCardDef) cards

isDiscardable :: Card -> Bool
isDiscardable = not . isWeakness
 where
  isWeakness = \case
    PlayerCard pc   -> isJust $ cdCardSubType $ toCardDef pc
    EncounterCard _ -> True -- maybe?
