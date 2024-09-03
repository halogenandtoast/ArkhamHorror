{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaign.Option where

import Arkham.Prelude

import Data.Aeson.TH

data CampaignOption
  = -- | Night of the Zealot
    AddLitaChantler
  | -- | The Dunwich Legacy
    Cheated
  | TakeArmitage
  | TakeWarrenRice
  | TakeFrancisMorgan
  | TakeZebulonWhately
  | TakeEarlSawyer
  | TakePowderOfIbnGhazi
  | TakeTheNecronomicon
  | AddAcrossTimeAndSpace
  | -- | The Circle Undone
    TakeBlackBook
  | TakePuzzleBox
  | ProceedToInterlude3
  | DebugOption
  deriving stock (Eq, Show, Ord, Data)

$(deriveJSON defaultOptions ''CampaignOption)

instance ToJSONKey CampaignOption
instance FromJSONKey CampaignOption
