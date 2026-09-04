-- | The runner behind a debug-authored custom event. See "Arkham.Custom.Enemy".
module Arkham.Custom.Event (CustomEvent (..), customEvent) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Event.Import.Lifted

newtype CustomEvent = CustomEvent EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customEvent :: CardDef -> EventCard CustomEvent
customEvent = event CustomEvent

instance RunMessage CustomEvent where
  runMessage msg (CustomEvent attrs) = CustomEvent <$> runMessage msg attrs
