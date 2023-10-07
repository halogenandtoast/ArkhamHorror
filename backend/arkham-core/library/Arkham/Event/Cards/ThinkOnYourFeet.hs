module Arkham.Event.Cards.ThinkOnYourFeet (
  thinkOnYourFeet,
  ThinkOnYourFeet (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Location
import Arkham.Movement

newtype ThinkOnYourFeet = ThinkOnYourFeet EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thinkOnYourFeet :: EventCard ThinkOnYourFeet
thinkOnYourFeet = event ThinkOnYourFeet Cards.thinkOnYourFeet

instance RunMessage ThinkOnYourFeet where
  runMessage msg e@(ThinkOnYourFeet attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      connectedLocations <- accessibleLocations iid
      pushWhen (notNull connectedLocations)
        $ chooseOrRunOne iid
        $ targetLabels connectedLocations (only . Move . move attrs iid)
      pure e
    _ -> ThinkOnYourFeet <$> runMessage msg attrs
