module Arkham.Helpers.Event where

import Arkham.Prelude

import Arkham.Target
import Arkham.Event.Attrs
import Arkham.Message
import Arkham.GameEnv

unshiftEffect :: EventAttrs -> Target -> GameT ()
unshiftEffect attrs target =
  pushAll
    [ CreateEffect (cdCardCode $ toCardDef attrs) Nothing (toSource attrs) target
    , Discard $ toTarget attrs
    ]
