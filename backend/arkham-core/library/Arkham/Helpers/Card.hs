module Arkham.Helpers.Card where

import Arkham.Prelude

import Arkham.Card
import Arkham.Projection
import Arkham.Matcher
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Classes.Query
import Arkham.Campaign.Attrs ( Field (..) )
import Arkham.Scenario.Attrs ( Field (..) )

getCampaignStoryCards :: GameT [PlayerCard]
getCampaignStoryCards = do
  mCampaignId <- selectOne TheCampaign
  case mCampaignId of
    Just campaignId -> fieldMap CampaignStoryCards (concat . toList) campaignId
    Nothing -> scenarioFieldMap ScenarioStoryCards (concat . toList)

getCampaignStoryCard :: CardDef -> GameT PlayerCard
getCampaignStoryCard def = do
  cards <- getCampaignStoryCards
  pure . fromJustNote "missing card" $ find ((== def) . toCardDef) cards

isDiscardable :: Card -> Bool
isDiscardable = not . isWeakness
 where
  isWeakness = \case
    PlayerCard pc   -> isJust $ cdCardSubType $ toCardDef pc
    EncounterCard _ -> True -- maybe?
