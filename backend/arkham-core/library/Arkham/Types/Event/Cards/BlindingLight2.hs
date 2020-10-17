{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.BlindingLight2 where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Token as Token
import Arkham.Types.TokenResponse
import Lens.Micro

import ClassyPrelude

newtype BlindingLight2 = BlindingLight2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

blindingLight2 :: InvestigatorId -> EventId -> BlindingLight2
blindingLight2 iid uuid = BlindingLight2 $ baseAttrs iid uuid "01069"

instance HasActions env BlindingLight2 where
  getActions i window (BlindingLight2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env BlindingLight2 where
  runMessage msg (BlindingLight2 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages
        [ ChooseEvadeEnemy
          iid
          (EventSource eid)
          SkillWillpower
          [Damage EnemyJustEvadedTarget (EventSource eventId) 2]
          []
          [ OnAnyToken
              [ Token.Skull
              , Token.Cultist
              , Token.Tablet
              , Token.ElderThing
              , Token.AutoFail
              ]
              [ LoseActions iid (EventSource eid) 1
              , InvestigatorAssignDamage iid (EventSource eid) 0 1
              ]
          ]
          False
        , Discard (EventTarget eid)
        ]
      BlindingLight2 <$> runMessage msg (attrs & resolved .~ True)
    _ -> BlindingLight2 <$> runMessage msg attrs
