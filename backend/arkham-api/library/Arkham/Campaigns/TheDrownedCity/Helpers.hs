module Arkham.Campaigns.TheDrownedCity.Helpers where

import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, push)
import Arkham.Effect.Types (makeEffectBuilder)
import Arkham.I18n
import Arkham.Id
import Arkham.Message (Message (CreateEffect))
import Arkham.Prelude
import Arkham.Source
import Arkham.Tracing

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theDrownedCity" a

struggleForAir :: (Sourceable a, HasGame m, Tracing m, HasQueue Message m) => a -> InvestigatorId -> m ()
struggleForAir a iid = do
  builder <- makeEffectBuilder "struggleForAir" Nothing a iid
  push $ CreateEffect builder
