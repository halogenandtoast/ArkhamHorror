module Arkham.Campaigns.BrethrenOfAsh.Helpers where

import Arkham.I18n
import Arkham.Id
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Source

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "brethrenOfAsh" a

codex :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
codex iid (toSource -> source) n = scenarioSpecific "codex" (iid, source, n)
