module Arkham.Helpers.Event where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.Classes.HasQueue
import Arkham.Event.Types
import Arkham.GameEnv
import Arkham.Message
import Arkham.Source
import Arkham.Target

unshiftEffect :: (Targetable target) => EventAttrs -> target -> GameT ()
unshiftEffect attrs (toTarget -> target) = pushAll [CreateEffect (cdCardCode $ toCardDef attrs) Nothing (toSource attrs) target]
