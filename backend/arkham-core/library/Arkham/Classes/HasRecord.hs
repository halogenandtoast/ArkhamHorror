{-# LANGUAGE DefaultSignatures #-}
module Arkham.Classes.HasRecord where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card.CardCode

class HasRecord m a where
  hasRecord :: CampaignLogKey -> a -> m Bool
  hasRecordSet :: CampaignLogKey -> a -> m [Recorded CardCode]
  hasRecordCount :: CampaignLogKey -> a -> m Int

instance HasRecord m a => HasRecord m (a `With` b) where
  hasRecord logKey = hasRecord logKey . withBase
  hasRecordSet logKey = hasRecordSet logKey . withBase
  hasRecordCount logKey = hasRecordCount logKey . withBase
