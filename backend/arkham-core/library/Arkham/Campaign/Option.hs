{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaign.Option where

import Arkham.Prelude

import Data.Aeson.TH

data CampaignOption
  = -- | Night of the Zealot
    AddLitaChantler
  | -- | The Circle Undone
    TakeBlackBook
  | TakePuzzleBox
  | ProceedToInterlude3
  | DebugOption
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NoThunks)

$(deriveJSON defaultOptions ''CampaignOption)

instance ToJSONKey CampaignOption
instance FromJSONKey CampaignOption
