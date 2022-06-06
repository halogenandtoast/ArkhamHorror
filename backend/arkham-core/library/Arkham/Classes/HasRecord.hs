{-# LANGUAGE DefaultSignatures #-}
module Arkham.Classes.HasRecord where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import {-# SOURCE #-} Arkham.GameEnv

class HasRecord a where
  hasRecord :: CampaignLogKey -> a -> GameT Bool
  hasRecordSet :: CampaignLogKey -> a -> GameT [Recorded CardCode]
  hasRecordCount :: CampaignLogKey -> a -> GameT Int

instance HasRecord a => HasRecord (a `With` b) where
  hasRecord logKey = hasRecord logKey . withBase
  hasRecordSet logKey = hasRecordSet logKey . withBase
  hasRecordCount logKey = hasRecordCount logKey . withBase
