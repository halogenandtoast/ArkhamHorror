module Arkham.Campaigns.ThePathToCarcosa.Helpers where

import Arkham.Prelude

import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.CampaignLogKey

getConviction :: GameT Int
getConviction = getRecordCount Conviction

getDoubt :: GameT Int
getDoubt = getRecordCount Doubt

getMoreConvictionThanDoubt :: GameT Bool
getMoreConvictionThanDoubt = liftA2 (>) getConviction getDoubt
