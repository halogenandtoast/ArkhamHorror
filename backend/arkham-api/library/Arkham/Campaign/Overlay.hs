{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Campaign.Overlay where

import Arkham.Card.CardCode
import Arkham.Id
import Arkham.Prelude

{- | Campaign-scoped changes to a side story. Availability controls entry cost;
activation persists for replacement reward cards in subsequent scenarios.
Replacement rules use the campaign hooks; canonical identity is preserved so
original scenario setup and reward matchers still work.
-}
data CampaignOverlay = CampaignOverlay
  { id :: Text
  , name :: Text
  , scenario :: ScenarioId
  , available :: Bool
  , active :: Bool
  , xpCost :: Int
  , cardReplacements :: Map CardCode CardCode
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
