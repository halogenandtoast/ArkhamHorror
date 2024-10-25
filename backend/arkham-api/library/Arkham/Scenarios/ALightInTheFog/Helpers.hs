module Arkham.Scenarios.ALightInTheFog.Helpers where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Classes.HasQueue
import Arkham.I18n
import Arkham.Id
import Arkham.Message (Message (ForInvestigator, ScenarioSpecific))
import Arkham.Message.Lifted
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "aLightInTheFog" a

captured :: ReverseQueue m => InvestigatorId -> m ()
captured iid = push $ ForInvestigator iid $ ScenarioSpecific "captured" Null
