module Arkham.Event.Cards.WorkingAHunch where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Location.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection

newtype WorkingAHunch = WorkingAHunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

workingAHunch :: EventCard WorkingAHunch
workingAHunch = event WorkingAHunch Cards.workingAHunch

instance RunMessage WorkingAHunch where
  runMessage msg e@(WorkingAHunch attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      currentLocationId <- getJustLocation iid
      locationClueCount <- field LocationClues currentLocationId
      if locationClueCount > 0
        then pushAll
          [ InvestigatorDiscoverClues iid currentLocationId 1 Nothing
          , discard attrs
          ]
        else push $ discard attrs
      pure e
    _ -> WorkingAHunch <$> runMessage msg attrs
