{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.BlindingLight where

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

newtype BlindingLight = BlindingLight Attrs
  deriving newtype (Show, ToJSON, FromJSON)

blindingLight :: InvestigatorId -> EventId -> BlindingLight
blindingLight iid uuid = BlindingLight $ baseAttrs iid uuid "01066"

instance HasActions env investigator BlindingLight where
  getActions i window (BlindingLight attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env BlindingLight where
  runMessage msg (BlindingLight attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      unshiftMessages
        [ ChooseEvadeEnemy
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
        , Discard (EventTarget eid)
        ]
      BlindingLight <$> runMessage msg (attrs & resolved .~ True)
    _ -> BlindingLight <$> runMessage msg attrs
