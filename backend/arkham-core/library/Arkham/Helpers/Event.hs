module Arkham.Helpers.Event where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Classes.Entity
import Arkham.Classes.HasQueue
import Arkham.Event.Types
import Arkham.GameEnv
import Arkham.Message
import Arkham.Source
import Arkham.Target

unshiftEffect :: EventAttrs -> Target -> GameT ()
unshiftEffect attrs target = pushAll
  [ CreateEffect (toCardCode attrs) Nothing (toSource attrs) target
  , Discard GameSource $ toTarget attrs
  ]
