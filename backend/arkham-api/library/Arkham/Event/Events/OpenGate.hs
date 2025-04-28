module Arkham.Event.Events.OpenGate (openGate) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Placement

newtype OpenGate = OpenGate EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openGate :: EventCard OpenGate
openGate = event OpenGate Cards.openGate

instance HasModifiersFor OpenGate where
  getModifiersFor (OpenGate a) = case a.placement of
    AttachedToLocation lid ->
      modified_
        a
        lid
        [ ConnectedToWhen
            (LocationWithId lid)
            (not_ (LocationWithId lid) <> LocationWithAttachedEvent (eventIs Cards.openGate))
        ]
    _ -> pure ()

instance RunMessage OpenGate where
  runMessage msg e@(OpenGate attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid $ place attrs . AttachedToLocation
      pure e
    _ -> OpenGate <$> liftRunMessage msg attrs
