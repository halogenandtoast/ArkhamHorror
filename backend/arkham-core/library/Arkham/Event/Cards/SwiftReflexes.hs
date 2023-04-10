module Arkham.Event.Cards.SwiftReflexes (
  swiftReflexes,
  SwiftReflexes (..),
) where

import Arkham.Action.Additional
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Query
import Arkham.Message
import Arkham.Prelude

newtype SwiftReflexes = SwiftReflexes EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swiftReflexes :: EventCard SwiftReflexes
swiftReflexes =
  event SwiftReflexes Cards.swiftReflexes

-- This card may be a little wonky, we want to take an action but we can't make
-- everything free as something that cost two actions would now be doable, but
-- the action should not count towards anything.
instance RunMessage SwiftReflexes where
  runMessage msg e@(SwiftReflexes attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      iid' <- getActiveInvestigatorId
      pushAll $
        [SetActiveInvestigator iid | iid /= iid']
          <> [ GainAdditionalAction iid (toSource attrs) AnyAdditionalAction
             , PlayerWindow iid [] False
             ]
          <> [SetActiveInvestigator iid' | iid /= iid']
      pure e
    _ -> SwiftReflexes <$> runMessage msg attrs
