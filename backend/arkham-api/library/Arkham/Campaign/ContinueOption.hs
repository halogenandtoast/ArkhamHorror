{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Campaign.ContinueOption where

import Arkham.Prelude

{- | An extra action a campaign offers on the continuation screen, beside
Continue and Upgrade Decks. Choosing one answers with
@CampaignOptionStep \<key\> \<a continuation that redraws this screen\>@, so the
campaign handles it and the table lands back on the same screen afterwards.
-}
data ContinueOption = ContinueOption
  { key :: Text
  -- ^ Passed back as the 'Arkham.CampaignStep.CampaignOptionStep' key.
  , label :: Text
  -- ^ A full i18n key, since a homebrew campaign owns its own locale namespace.
  , available :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
