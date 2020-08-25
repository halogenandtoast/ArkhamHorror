module Arkham.Types.Event.Cards.Backstab where

import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import ClassyPrelude

backstab
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
backstab iid = unshiftMessage
  (ChooseFightEnemy
    iid
    SkillAgility
    [DamageDealt 2 (EventSource "01051")]
    mempty
    False
  )
