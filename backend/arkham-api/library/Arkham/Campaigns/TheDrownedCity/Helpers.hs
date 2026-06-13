module Arkham.Campaigns.TheDrownedCity.Helpers where

import Arkham.I18n
import Arkham.Prelude

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theDrownedCity" a
