module Arkham.Helpers.Event where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.Classes.HasQueue
import Arkham.Event.Types
import Arkham.Message
import Arkham.Source
import Arkham.Target

unshiftEffect :: (HasQueue Message m, Targetable target) => EventAttrs -> target -> m ()
unshiftEffect attrs (toTarget -> target) = push $ createCardEffect (toCardDef attrs) Nothing (toSource attrs) target
