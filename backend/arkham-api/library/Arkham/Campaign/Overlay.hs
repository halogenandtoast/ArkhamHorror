{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Arkham.Campaign.Overlay where

import Arkham.Id
import Arkham.Prelude

{- | A campaign-scoped change to a side story: whether it is currently on offer
and at what entry cost. Rule changes the side story brings with it are
implemented by the campaign and its scenarios, not described here.
-}
data CampaignOverlay = CampaignOverlay
  { id :: Text
  , name :: Text
  , scenario :: ScenarioId
  , available :: Bool
  , xpCost :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
