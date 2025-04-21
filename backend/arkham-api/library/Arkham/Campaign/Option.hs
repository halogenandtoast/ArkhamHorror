{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaign.Option where

import Arkham.Prelude

import Data.Aeson.TH

data CampaignOption
  = PerformIntro
  | -- | Night of the Zealot
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
  | AddAcrossSpaceAndTime
  | -- | The Circle Undone
    TakeBlackBook
  | TakePuzzleBox
  | ProceedToInterlude3
  | DebugOption
  | -- | The Edge of the Earth
    ManuallyPickCamp
  | ManuallyPickKilledInPlaneCrash
  | AddGreenSoapstone
  | AddWoodenSledge
  | AddDynamite
  | AddMiasmicCrystal
  | AddMineralSpecimen
  | AddSmallRadio
  | AddSpareParts
  | IncludePartners
  | FatalMiragePart1
  | FatalMiragePart2
  | FatalMiragePart3
  deriving stock (Eq, Show, Ord, Data)

$(deriveJSON defaultOptions ''CampaignOption)

instance ToJSONKey CampaignOption
instance FromJSONKey CampaignOption
