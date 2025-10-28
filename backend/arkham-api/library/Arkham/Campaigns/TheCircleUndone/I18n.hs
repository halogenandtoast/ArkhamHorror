module Arkham.Campaigns.TheCircleUndone.I18n where

import Arkham.I18n
import Arkham.Prelude

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theCircleUndone" a
