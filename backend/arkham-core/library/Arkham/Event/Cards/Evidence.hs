module Arkham.Event.Cards.Evidence where

import Arkham.Prelude

import Arkham.Classes
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
  runMessage msg e@(Evidence attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      currentLocationId <- getJustLocation iid
      hasClues <- fieldP LocationClues (> 0) currentLocationId
      pushAll $
        [ InvestigatorDiscoverClues iid currentLocationId (toSource attrs) 1 Nothing
        | hasClues
        ]
      pure e
    _ -> Evidence <$> runMessage msg attrs
