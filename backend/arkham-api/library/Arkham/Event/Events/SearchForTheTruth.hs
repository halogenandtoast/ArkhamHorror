module Arkham.Event.Events.SearchForTheTruth (searchForTheTruth) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype SearchForTheTruth = SearchForTheTruth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheTruth :: EventCard SearchForTheTruth
searchForTheTruth = event SearchForTheTruth Cards.searchForTheTruth

instance RunMessage SearchForTheTruth where
  runMessage msg e@(SearchForTheTruth attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      x <- fieldMap InvestigatorClues (min 5) iid
      drawCards iid attrs x
      pure e
    _ -> SearchForTheTruth <$> liftRunMessage msg attrs
