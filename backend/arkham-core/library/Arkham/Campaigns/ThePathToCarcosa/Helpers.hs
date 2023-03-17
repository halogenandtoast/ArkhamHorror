module Arkham.Campaigns.ThePathToCarcosa.Helpers where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Log
import Arkham.Message

getConviction :: HasGame m => m Int
getConviction = getRecordCount Conviction

getDoubt :: HasGame m => m Int
getDoubt = getRecordCount Doubt

getMoreConvictionThanDoubt :: HasGame m => m Bool
getMoreConvictionThanDoubt = liftA2 (>) getConviction getDoubt

markConviction :: HasGame m => m Message
markConviction = do
  n <- getConviction
  pure $ RecordCount Conviction (n + 1)

markDoubt :: HasGame m => m Message
markDoubt = do
  n <- getDoubt
  pure $ RecordCount Doubt (n + 1)

interviewed :: HasGame m => CardDef -> m Bool
interviewed assetDef =
  elem (recorded $ toCardCode assetDef) <$> getRecordSet VIPsInterviewed

slain :: (HasGame m, HasCardCode cardCode) => cardCode -> m Bool
slain (toCardCode -> cardCode) =
  elem (recorded cardCode) <$> getRecordSet VIPsSlain
