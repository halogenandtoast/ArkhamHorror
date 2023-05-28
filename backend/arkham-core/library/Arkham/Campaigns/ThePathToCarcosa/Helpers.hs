module Arkham.Campaigns.ThePathToCarcosa.Helpers where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Log
import Arkham.Message
import Arkham.Store

getConviction :: (HasGame m, Store m Card) => m Int
getConviction = getRecordCount Conviction

getDoubt :: (HasGame m, Store m Card) => m Int
getDoubt = getRecordCount Doubt

getMoreConvictionThanDoubt :: (HasGame m, Store m Card) => m Bool
getMoreConvictionThanDoubt = liftA2 (>) getConviction getDoubt

markConviction :: (HasGame m, Store m Card) => m Message
markConviction = do
  n <- getConviction
  pure $ RecordCount Conviction (n + 1)

markDoubt :: (HasGame m, Store m Card) => m Message
markDoubt = do
  n <- getDoubt
  pure $ RecordCount Doubt (n + 1)

interviewed :: (HasGame m, Store m Card) => CardDef -> m Bool
interviewed assetDef =
  elem (recorded $ toCardCode assetDef) <$> getRecordSet VIPsInterviewed

slain :: (HasGame m, Store m Card, HasCardCode cardCode) => cardCode -> m Bool
slain (toCardCode -> cardCode) =
  elem (recorded cardCode) <$> getRecordSet VIPsSlain
