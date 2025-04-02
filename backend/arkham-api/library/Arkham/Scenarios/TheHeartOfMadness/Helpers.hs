module Arkham.Scenarios.TheHeartOfMadness.Helpers where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.I18n
import Arkham.Prelude
import Arkham.Id

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "toTheForbiddenPeaks" a

sealAtLocationOf :: Applicative m => InvestigatorId -> m Bool
sealAtLocationOf _iid = pure False
