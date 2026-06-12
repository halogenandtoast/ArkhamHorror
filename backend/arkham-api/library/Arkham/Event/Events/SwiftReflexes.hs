module Arkham.Event.Events.SwiftReflexes (swiftReflexes, SwiftReflexes (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype SwiftReflexes = SwiftReflexes EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swiftReflexes :: EventCard SwiftReflexes
swiftReflexes = event SwiftReflexes Cards.swiftReflexes

instance RunMessage SwiftReflexes where
  runMessage msg e@(SwiftReflexes attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      -- Same "additional action this turn" language as Quick Thinking, so it
      -- uses the same takeActionAsIfTurn mechanism (gains the action and opens
      -- an immediate, as-if-it-were-your-turn window with no fast window).
      takeActionAsIfTurn iid attrs
      pure e
    _ -> SwiftReflexes <$> liftRunMessage msg attrs
