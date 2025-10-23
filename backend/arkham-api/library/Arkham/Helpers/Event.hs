module Arkham.Helpers.Event where

import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Event.Types
import Arkham.Helpers.Effect
import Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Tracing

unshiftEffect
  :: (HasQueue Message m, HasGame m, Tracing m, Targetable target) => EventAttrs -> target -> m ()
unshiftEffect attrs (toTarget -> target) = push =<< createCardEffect (toCardDef attrs) Nothing (toSource attrs) target
