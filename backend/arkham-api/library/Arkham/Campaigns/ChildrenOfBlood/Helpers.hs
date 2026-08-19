module Arkham.Campaigns.ChildrenOfBlood.Helpers where

import Arkham.I18n
import Arkham.Prelude

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "childrenOfBlood" a
