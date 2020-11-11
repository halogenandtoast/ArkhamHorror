{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.BlindingLight where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import qualified Arkham.Types.Token as Token
import Arkham.Types.TokenResponse

newtype BlindingLight = BlindingLight Attrs
  deriving newtype (Show, ToJSON, FromJSON)

blindingLight :: InvestigatorId -> EventId -> BlindingLight
blindingLight iid uuid = BlindingLight $ baseAttrs iid uuid "01066"

instance HasModifiersFor env BlindingLight where
  getModifiersFor = noModifiersFor

instance HasActions env BlindingLight where
  getActions i window (BlindingLight attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env BlindingLight where
  runMessage msg (BlindingLight attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages
        [ ChooseEvadeEnemy
          iid
          (EventSource eid)
          SkillWillpower
          [Damage EnemyJustEvadedTarget (EventSource eid) 1]
          []
          [ OnAnyToken
              [ Token.Skull
              , Token.Cultist
              , Token.Tablet
              , Token.ElderThing
              , Token.AutoFail
              ]
              [LoseActions iid (EventSource eid) 1]
          ]
          False
        , Discard (EventTarget eid)
        ]
      BlindingLight <$> runMessage msg (attrs & resolved .~ True)
    _ -> BlindingLight <$> runMessage msg attrs
