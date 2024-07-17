module Arkham.Event.Cards.PreparedForTheWorst2 (preparedForTheWorst2, PreparedForTheWorst2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Capability
import Arkham.Strategy

newtype PreparedForTheWorst2 = PreparedForTheWorst2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preparedForTheWorst2 :: EventCard PreparedForTheWorst2
preparedForTheWorst2 = event PreparedForTheWorst2 Cards.preparedForTheWorst2

instance RunMessage PreparedForTheWorst2 where
  runMessage msg e@(PreparedForTheWorst2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      selectWithNonNull (affectsOthers $ colocatedWith iid <> can.search.deck) $
        chooseOneToHandle iid attrs
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      search iid attrs iid [fromTopOfDeck 9] (basic $ #asset <> #weapon) (AddToHandOrPlayFound iid 1)
      pure e
    _ -> PreparedForTheWorst2 <$> liftRunMessage msg attrs
