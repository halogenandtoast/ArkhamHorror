module Arkham.Campaigns.ChildrenOfBlood.Helpers where

import Arkham.I18n
import Arkham.Id
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Source

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "childrenOfBlood" a

codex :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
codex iid (toSource -> source) n = scenarioSpecific "codex" (iid, source, n)

-- | A codex entry that acts on a particular enemy (Blood Money's party guests).
codexOn :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> EnemyId -> m ()
codexOn iid (toSource -> source) n eid = scenarioSpecific "codexOn" (iid, source, n, eid)
