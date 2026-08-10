module Arkham.Campaigns.ThePathToCarcosa.Helpers where

import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.Key
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Helpers.Log
import Arkham.I18n
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Queue
import Arkham.Prelude

getConviction :: HasGame m => m Int
getConviction = getRecordCount Conviction

getDoubt :: HasGame m => m Int
getDoubt = getRecordCount Doubt

getMoreConvictionThanDoubt :: HasGame m => m Bool
getMoreConvictionThanDoubt = liftA2 (>) getConviction getDoubt

markConviction :: ReverseQueue m => m ()
markConviction = markConvictionN 1
{-# INLINE markConviction #-}

markDoubt :: ReverseQueue m => m ()
markDoubt = markDoubtN 1
{-# INLINE markDoubt #-}

markDoubtN :: ReverseQueue m => Int -> m ()
markDoubtN x = do
  n <- getDoubt
  recordCount Doubt (n + x)

markConvictionN :: ReverseQueue m => Int -> m ()
markConvictionN x = do
  n <- getConviction
  recordCount Conviction (n + x)

interviewed :: HasGame m => CardDef -> m Bool
interviewed assetDef =
  elem (recorded $ toCardCode assetDef) <$> getRecordSet VIPsInterviewed

whenInterviewed :: HasGame m => CardDef -> m () -> m ()
whenInterviewed assetDef = whenM (interviewed assetDef)

slain :: (HasGame m, HasCardCode cardCode) => cardCode -> m Bool
slain (toCardCode -> cardCode) = elem (recorded cardCode) <$> getRecordSet VIPsSlain

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "thePathToCarcosa" a
