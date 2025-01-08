module Arkham.Event.Events.OpenGate (openGate, OpenGate (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype OpenGate = OpenGate EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openGate :: EventCard OpenGate
openGate = event OpenGate Cards.openGate

instance HasModifiersFor OpenGate where
  getModifiersFor (OpenGate a) =
    case a.placement of
      AttachedToLocation lid -> do
        modifySelectMap a (not_ (LocationWithId lid) <> LocationWithAttachedEvent (eventIs Cards.openGate)) \lid' ->
          [ConnectedToWhen (LocationWithId lid) (LocationWithId lid')]
      _ -> pure mempty

instance RunMessage OpenGate where
  runMessage msg e@(OpenGate attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      lid <- getJustLocation iid
      push $ PlaceEvent eid (AttachedToLocation lid)
      pure e
    _ -> OpenGate <$> runMessage msg attrs
