{-# LANGUAGE DefaultSignatures #-}
module Arkham.Classes.HasRecord where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card.CardCode

class HasRecord env a where
  hasRecord :: MonadReader env m => CampaignLogKey -> a -> m Bool
  hasRecordSet :: MonadReader env m => CampaignLogKey -> a -> m [Recorded CardCode]
  hasRecordCount :: MonadReader env m => CampaignLogKey -> a -> m Int

instance HasRecord env a => HasRecord env (a `With` b) where
  hasRecord logKey = hasRecord logKey . withBase
  hasRecordSet logKey = hasRecordSet logKey . withBase
  hasRecordCount logKey = hasRecordCount logKey . withBase
