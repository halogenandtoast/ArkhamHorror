module Arkham.Event.Events.ThinkOnYourFeet (thinkOnYourFeet) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Message.Lifted.Move

newtype ThinkOnYourFeet = ThinkOnYourFeet EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thinkOnYourFeet :: EventCard ThinkOnYourFeet
thinkOnYourFeet = event ThinkOnYourFeet Cards.thinkOnYourFeet

instance RunMessage ThinkOnYourFeet where
  runMessage msg e@(ThinkOnYourFeet attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      connectedLocations <- getAccessibleLocations iid attrs
      chooseTargetM iid connectedLocations (moveTo attrs iid)
      pure e
    _ -> ThinkOnYourFeet <$> liftRunMessage msg attrs
