module Arkham.Campaigns.ThePathToCarcosa.Helpers where

import Arkham.Prelude

import Arkham.Classes.HasRecord
import Arkham.Game.Helpers
import Arkham.CampaignLogKey

getConviction :: (HasRecord env (), MonadReader env m) => m Int
getConviction = getRecordCount Conviction

getDoubt :: (HasRecord env (), MonadReader env m) => m Int
getDoubt = getRecordCount Doubt

getMoreConvictionThanDoubt :: (HasRecord env (), MonadReader env m) => m Bool
getMoreConvictionThanDoubt = liftA2 (>) getConviction getDoubt
