module Arkham.Types.Classes.HasRecord where

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode
import ClassyPrelude

class HasRecord a where
  hasRecord :: CampaignLogKey -> a -> Bool
  hasRecordSet :: CampaignLogKey -> a -> [CardCode]
