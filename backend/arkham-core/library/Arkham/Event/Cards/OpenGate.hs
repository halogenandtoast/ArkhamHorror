module Arkham.Event.Cards.OpenGate (
  openGate,
  OpenGate (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Placement

newtype OpenGate = OpenGate EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

openGate :: EventCard OpenGate
openGate = event OpenGate Cards.openGate

instance HasModifiersFor OpenGate where
  getModifiersFor (LocationTarget lid) (OpenGate a) =
    case eventPlacement a of
      AttachedToLocation lid' | lid /= lid' -> do
        hasGate <- selectAny $ EventAt (LocationWithId lid) <> eventIs Cards.openGate
        pure $ toModifiers a [ConnectedToWhen (LocationWithId lid) (LocationWithId lid') | hasGate]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage OpenGate where
  runMessage msg e@(OpenGate attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      lid <- getJustLocation iid
      push $ PlaceEvent iid eid (AttachedToLocation lid)
      pure e
    _ -> OpenGate <$> runMessage msg attrs
