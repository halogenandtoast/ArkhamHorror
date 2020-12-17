module Arkham.Types.Classes.HasRecord where

import Arkham.Prelude

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode

class HasRecord a where
  hasRecord :: CampaignLogKey -> a -> Bool
  hasRecordSet :: CampaignLogKey -> a -> [CardCode]
