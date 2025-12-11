module Arkham.Scenarios.WithoutATrace.Helpers where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Card.CardCode
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher.Window
import Arkham.Message (Message)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Queue
import Arkham.Source

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "withoutATrace" a

exposedInShadows
  :: (Sourceable a, HasCardCode a, ReverseQueue m) => InvestigatorId -> a -> QueueT Message m () -> m ()
exposedInShadows iid source = chooseOneM iid . abilityLabeled iid (mkAbility source (-1) $ forced AnyWindow)
