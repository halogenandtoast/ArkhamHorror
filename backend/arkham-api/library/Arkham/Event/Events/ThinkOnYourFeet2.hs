module Arkham.Event.Events.ThinkOnYourFeet2 (thinkOnYourFeet2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Movement

newtype ThinkOnYourFeet2 = ThinkOnYourFeet2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thinkOnYourFeet2 :: EventCard ThinkOnYourFeet2
thinkOnYourFeet2 = event ThinkOnYourFeet2 Cards.thinkOnYourFeet2

instance RunMessage ThinkOnYourFeet2 where
  runMessage msg e@(ThinkOnYourFeet2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      connectedLocations <- getAccessibleLocations iid attrs
      when (notNull connectedLocations) do
        chooseOrRunOne iid $ targetLabels connectedLocations (only . Move . move attrs iid)
      pure e
    _ -> ThinkOnYourFeet2 <$> liftRunMessage msg attrs
