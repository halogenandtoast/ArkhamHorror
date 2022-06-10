module Arkham.Campaigns.ThePathToCarcosa.Helpers where

import Arkham.Prelude

import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.CampaignLogKey

getConviction :: (Monad m, HasGame m) => m Int
getConviction = getRecordCount Conviction

getDoubt :: (Monad m, HasGame m) => m Int
getDoubt = getRecordCount Doubt

getMoreConvictionThanDoubt :: (Monad m, HasGame m) => m Bool
getMoreConvictionThanDoubt = liftA2 (>) getConviction getDoubt
