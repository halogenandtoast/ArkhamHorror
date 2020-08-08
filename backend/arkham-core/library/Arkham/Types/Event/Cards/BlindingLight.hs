module Arkham.Types.Event.Cards.BlindingLight where

import Arkham.Types.InvestigatorId
import Arkham.Types.Classes
import Arkham.Types.Source
import Arkham.Types.SkillType
import Arkham.Types.TokenResponse
import Arkham.Types.Message
import Arkham.Types.GameRunner
import qualified Arkham.Types.Token as Token
import ClassyPrelude

blindingLight
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
blindingLight iid = unshiftMessage
  (ChooseEvadeEnemy
    iid
    SkillWillpower
    []
    []
    [ OnAnyToken
        [ Token.Skull
        , Token.Cultist
        , Token.Tablet
        , Token.ElderThing
        , Token.AutoFail
        ]
        [LoseAction iid (EventSource "01066")]
    ]
    False
  )

