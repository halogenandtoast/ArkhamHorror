module Arkham.Campaigns.ThePathToCarcosa.Helpers where

import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Helpers.Log
import Arkham.I18n
import Arkham.Message.Lifted (recordCount)
import Arkham.Message.Lifted.Queue
import Arkham.Prelude

getConviction :: HasGame m => m Int
getConviction = getRecordCount Conviction

getDoubt :: HasGame m => m Int
getDoubt = getRecordCount Doubt

getMoreConvictionThanDoubt :: HasGame m => m Bool
getMoreConvictionThanDoubt = liftA2 (>) getConviction getDoubt

markConviction :: ReverseQueue m => m ()
markConviction = do
  n <- getConviction
  recordCount Conviction (n + 1)

markDoubt :: ReverseQueue m => m ()
markDoubt = do
  n <- getDoubt
  recordCount Doubt (n + 1)

interviewed :: HasGame m => CardDef -> m Bool
interviewed assetDef =
  elem (recorded $ toCardCode assetDef) <$> getRecordSet VIPsInterviewed

whenInterviewed :: HasGame m => CardDef -> m () -> m ()
whenInterviewed assetDef = whenM (interviewed assetDef)

slain :: (HasGame m, HasCardCode cardCode) => cardCode -> m Bool
slain (toCardCode -> cardCode) =
  elem (recorded cardCode) <$> getRecordSet VIPsSlain

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "thePathToCarcosa" a
