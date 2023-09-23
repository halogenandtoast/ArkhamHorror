module Arkham.Event.Cards.Evidence where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Message
import Arkham.Projection

newtype Evidence = Evidence EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evidence :: EventCard Evidence
evidence = event Evidence Cards.evidence

instance RunMessage Evidence where
  runMessage msg e@(Evidence attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      currentLocation <- getJustLocation iid
      pushWhenM (fieldSome LocationClues currentLocation)
        $ toMessage
        $ discover iid currentLocation attrs 1
      pure e
    _ -> Evidence <$> runMessage msg attrs
