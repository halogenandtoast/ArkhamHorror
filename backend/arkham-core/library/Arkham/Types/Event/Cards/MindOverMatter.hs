module Arkham.Types.Event.Cards.MindOverMatter where

import Arkham.Types.InvestigatorId
import Arkham.Types.Classes
import Arkham.Types.Source
import Arkham.Types.SkillType
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.GameRunner
import ClassyPrelude

mindOverMatter
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
mindOverMatter iid = unshiftMessages
  [ AddModifier
    (InvestigatorTarget iid)
    (UseSkillInPlaceOf SkillCombat SkillIntellect (EventSource "01036"))
  , AddModifier
    (InvestigatorTarget iid)
    (UseSkillInPlaceOf SkillAgility SkillIntellect (EventSource "01036"))
  ]
