module Arkham.Types.Classes.HasRecord where

import Arkham.Types.CampaignLogKey
import ClassyPrelude

class HasRecord a where
  hasRecord :: CampaignLogKey -> a -> Bool
