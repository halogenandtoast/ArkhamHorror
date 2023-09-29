module Arkham.Event.Cards.WorkingAHunch where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Projection

newtype WorkingAHunch = WorkingAHunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

workingAHunch :: EventCard WorkingAHunch
workingAHunch = event WorkingAHunch Cards.workingAHunch

instance RunMessage WorkingAHunch where
  runMessage msg e@(WorkingAHunch attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      current <- getJustLocation iid
      pushWhenM (fieldSome LocationClues current)
        $ toMessage
        $ discover iid current attrs 1
      pure e
    _ -> WorkingAHunch <$> runMessage msg attrs
